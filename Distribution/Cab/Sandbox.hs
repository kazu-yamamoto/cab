{-# LANGUAGE BangPatterns #-}

module Distribution.Cab.Sandbox (
    getSandbox
  , getSandboxOpts
  ) where

import Control.Applicative ((<$>))
import Control.Exception as E (catch, SomeException, throwIO)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Distribution.Cab.PkgDB
import Distribution.Simple.Program (ghcProgram)
import Distribution.Simple.Program.Types (programName, programFindVersion)
import Distribution.Verbosity (silent)
import Distribution.Version (versionBranch)
import System.Directory (getCurrentDirectory, doesFileExist)
import System.FilePath ((</>), takeDirectory)

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
    if exist then
        return cfile
      else do
        let dir' = takeDirectory dir
        if dir == dir' then
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
getSandboxOpts :: Maybe FilePath -> IO String
getSandboxOpts Nothing     = return ""
getSandboxOpts (Just path) = do
    ghcver <- ghcVersion
    pkgConf <- getPackageConf path
    let pkgOpt | ghcver >= 706 = "-package-db "
               | otherwise     = "-package-conf "
    return $ pkgOpt ++ pkgConf ++ " "

ghcVersion :: IO Int
ghcVersion = toInt <$> ghcVer
  where
    ghcVer = programFindVersion ghcProgram silent (programName ghcProgram)
    toInt Nothing = 0
    toInt (Just v)
      | length vs < 2 = 0
      | otherwise     = (vs !! 0) * 100 + (vs !! 1)
      where
        vs = versionBranch v
