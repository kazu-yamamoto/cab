{-# LANGUAGE OverloadedStrings #-}

module Distribution.Cab.GenPaths (genPaths) where

import Control.Exception
import Control.Monad
import Data.List (isSuffixOf)
import Distribution.Cab.Utils (readGenericPackageDescription, unPackageName)
import Distribution.Package (pkgName, pkgVersion)
import Distribution.PackageDescription (package, packageDescription)
import Distribution.Verbosity (silent)
import Distribution.Version
import System.Directory

genPaths :: IO ()
genPaths = do
    (nm, ver) <- getCabalFile >>= getNameVersion
    let file = "Paths_" ++ nm ++ ".hs"
    check file >> do
        putStrLn $ "Writing " ++ file ++ "..."
        writeFile file $
            "module Paths_"
                ++ nm
                ++ "  where\n"
                ++ "import Data.Version\n"
                ++ "\n"
                ++ "version :: Version\n"
                ++ "version = "
                ++ show ver
                ++ "\n"
  where
    check file = do
        exist <- doesFileExist file
        when exist . throwIO . userError $ file ++ " already exists"

getNameVersion :: FilePath -> IO (String, Version)
getNameVersion file = do
    desc <- readGenericPackageDescription silent file
    let pkg = package . packageDescription $ desc
        nm = unPackageName $ pkgName pkg
        name = map (trans '-' '_') nm
        version = pkgVersion pkg
    return (name, version)
  where
    trans c1 c2 c
        | c == c1 = c2
        | otherwise = c

getCabalFile :: IO FilePath
getCabalFile = do
    cnts <-
        (filter isCabal <$> getDirectoryContents ".")
            >>= filterM doesFileExist
    case cnts of
        [] -> throwIO $ userError "Cabal file does not exist"
        cfile : _ -> return cfile
  where
    isCabal :: String -> Bool
    isCabal nm = ".cabal" `isSuffixOf` nm && length nm > 6
