module PkgDB where

import Distribution.Version
import Distribution.InstalledPackageInfo
import Distribution.Package hiding (depends)
import Distribution.Simple.Compiler
import Distribution.Simple.GHC
import Distribution.Simple.PackageIndex
import Distribution.Simple.Program.Db
import Distribution.Verbosity
import Data.List
import System.Directory
import Types

getPkgDB :: IO PackageIndex
getPkgDB = do
    (_,pro) <- configure normal Nothing Nothing emptyProgramDb
    getInstalledPackages normal [GlobalPackageDB,UserPackageDB] pro

toPkgList :: PackageIndex -> (InstalledPackageInfo -> Bool) -> [InstalledPackageInfo]
toPkgList db prd = filter prd $ allPackages db

pkgName' :: InstalledPackageInfo -> String
pkgName' = toName . sourcePackageId

toName :: PackageId -> String
toName pid = name pid ++ " " ++ ver pid
  where
    name = toString . pkgName
    toString (PackageName x) = x
    ver = versionToString . pkgVersion

versionToString :: Version -> String
versionToString = concat . intersperse "." . map show . versionBranch

lookupByName :: String -> PackageIndex -> [(Version, [InstalledPackageInfo])]
lookupByName x db = lookupPackageName db (PackageName x)

printPkg :: String -> PackageIndex -> (Version, [InstalledPackageInfo]) -> IO ()
printPkg _ _ (_, []) = return ()
printPkg pkgnm db (ver, pkgi:_) = do
    putStrLn $ pkgnm ++ " " ++ versionToString ver
    mapM_ (printPkg' db) $ depends pkgi

printPkg' :: PackageIndex -> InstalledPackageId -> IO ()
printPkg' db pid = case lookupInstalledPackageId db pid of
    Nothing -> return ()
    Just pkgi -> putStrLn $ "    " ++ toName (sourcePackageId pkgi)

makeUserOnly :: IO (InstalledPackageInfo -> Bool)
makeUserOnly = do
    userDirPref <- getAppUserDataDirectory ""
    return $ \pkgi -> userDirPref `isPrefixOf` (head $ libraryDirs pkgi)

toPackageId :: OldPkg -> PackageId
toPackageId (OldPkg name cur _) = PackageIdentifier {
    pkgName = PackageName name
  , pkgVersion = Version {
        versionBranch = splitVersion cur
      , versionTags = []
      }
  }

splitVersion :: String -> [Int]
splitVersion [] = []
splitVersion xs = case break (=='.') xs of
    (x,"") -> [read x :: Int]
    (x,_:ys) -> (read x :: Int) : splitVersion ys

pkgVersion' :: InstalledPackageInfo -> Version
pkgVersion' = pkgVersion . sourcePackageId

pkgSrcId :: InstalledPackageInfo -> PackageId
pkgSrcId = sourcePackageId
