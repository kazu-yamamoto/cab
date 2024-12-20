{-# LANGUAGE BangPatterns #-}

module Distribution.Cab.Sandbox (
    getSandbox,
    getSandboxOpts,
    getSandboxOpts2,
) where

import Control.Exception as E (SomeException, catch, throwIO)
import Data.Char (isSpace)
import Data.List (isPrefixOf, tails)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (takeDirectory, takeFileName, (</>))

----------------------------------------------------------------

configFile :: String
configFile = "cabal.sandbox.config"

pkgDbKey :: String
pkgDbKey = "package-db:"

pkgDbKeyLen :: Int
pkgDbKeyLen = length pkgDbKey

-- | Find a sandbox config file by tracing ancestor directories,
--   parse it and return the package db path
getSandbox :: IO (Maybe FilePath)
getSandbox = (Just <$> getPkgDb) `E.catch` handler
  where
    getPkgDb = getCurrentDirectory >>= getSandboxConfigFile >>= getPackageDbDir
    handler :: SomeException -> IO (Maybe String)
    handler _ = return Nothing

-- | Find a sandbox config file by tracing ancestor directories.
--   Exception is thrown if not found
getSandboxConfigFile :: FilePath -> IO FilePath
getSandboxConfigFile dir = do
    let cfile = dir </> configFile
    exist <- doesFileExist cfile
    if exist
        then
            return cfile
        else do
            let dir' = takeDirectory dir
            if dir == dir'
                then
                    throwIO $ userError "sandbox config file not found"
                else
                    getSandboxConfigFile dir'

-- | Extract a package db directory from the sandbox config file.
--   Exception is thrown if the sandbox config file is broken.
getPackageDbDir :: FilePath -> IO FilePath
getPackageDbDir sconf = do
    -- Be strict to ensure that an error can be caught.
    !path <- extractValue . parse <$> readFile sconf
    return path
  where
    parse = head . filter ("package-db:" `isPrefixOf`) . lines
    extractValue = fst . break isSpace . dropWhile isSpace . drop pkgDbKeyLen

----------------------------------------------------------------

-- | Generate GHC options for package db according to GHC version.
--
-- >>> getSandboxOpts Nothing
-- ""
-- >>> getSandboxOpts (Just "/path/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d")
-- "-package-db /path/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"
-- >>> getSandboxOpts (Just "/path/.cabal-sandbox/i386-osx-ghc-7.4.1-packages.conf.d")
-- "-package-conf /path/.cabal-sandbox/i386-osx-ghc-7.4.1-packages.conf.d"
getSandboxOpts :: Maybe FilePath -> String
getSandboxOpts Nothing = ""
getSandboxOpts (Just path) = pkgOpt ++ path
  where
    ghcver = extractGhcVer path
    pkgOpt
        | ghcver >= 706 = "-package-db "
        | otherwise = "-package-conf "

getSandboxOpts2 :: Maybe FilePath -> String
getSandboxOpts2 Nothing = ""
getSandboxOpts2 (Just path) = pkgOpt ++ "=" ++ path
  where
    ghcver = extractGhcVer path
    pkgOpt
        | ghcver >= 706 = "--package-db"
        | otherwise = "--package-conf"

-- | Extracting GHC version from the path of package db.
--   Exception is thrown if the string argument is incorrect.
--
-- >>> extractGhcVer "/foo/bar/i386-osx-ghc-7.6.3-packages.conf.d"
-- 706
extractGhcVer :: String -> Int
extractGhcVer dir = ver
  where
    file = takeFileName dir
    findVer = drop 4 . head . filter ("ghc-" `isPrefixOf`) . tails
    (verStr1, left) = break (== '.') $ findVer file
    (verStr2, _) = break (== '.') $ tail left
    ver = read verStr1 * 100 + read verStr2
