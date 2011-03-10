module PkgDB where

import Distribution.Version
    (Version(..))
import Distribution.InstalledPackageInfo
    (InstalledPackageInfo_(..), InstalledPackageInfo)
import Distribution.Package
    (PackageName(..), PackageId, PackageIdentifier(..), InstalledPackageId)
import Distribution.Simple.Compiler
    (PackageDB(..))
import Distribution.Simple.GHC
    (configure, getInstalledPackages)
import Distribution.Simple.PackageIndex
    (lookupPackageName, lookupInstalledPackageId, allPackages, PackageIndex)
import Distribution.Simple.Program.Db
    (defaultProgramDb)
import Distribution.Verbosity
    (normal)
import Data.List
import System.Directory
import Utils

type PkgDB = PackageIndex
type PkgInfo = InstalledPackageInfo

----------------------------------------------------------------

getPkgDB :: IO PkgDB
getPkgDB = do
    (_,pro) <- configure normal Nothing Nothing defaultProgramDb
    getInstalledPackages normal [GlobalPackageDB,UserPackageDB] pro

lookupByName :: String -> PkgDB -> [PkgInfo]
lookupByName x db = map (head . snd) $ lookupPackageName db (PackageName x)

----------------------------------------------------------------

toPkgList :: PkgDB -> (PkgInfo -> Bool) -> [PkgInfo]
toPkgList db prd = filter prd $ allPackages db

makeUserOnly :: IO (PkgInfo -> Bool)
makeUserOnly = do
    userDirPref <- getAppUserDataDirectory ""
    return $ \pkgi -> userDirPref `isPrefixOf` (head $ libraryDirs pkgi)

----------------------------------------------------------------

nameOfPkgInfo :: PkgInfo -> String
nameOfPkgInfo = toName . sourcePackageId
  where
    toName pid = name pid ++ " " ++ ver pid
    name = toString . pkgName
    toString (PackageName x) = x
    ver = toDotted . versionBranch . pkgVersion

versionOfPkgInfo :: PkgInfo -> [Int]
versionOfPkgInfo = versionBranch . pkgVersion . sourcePackageId

idOfPkgInfo :: PkgInfo -> PackageId
idOfPkgInfo = sourcePackageId

----------------------------------------------------------------

printPkg :: PkgInfo -> PkgDB -> IO ()
printPkg pkgi db = do
    putStrLn $ nameOfPkgInfo pkgi
    mapM_ (printDep db) $ depends pkgi

printDep :: PkgDB -> InstalledPackageId -> IO ()
printDep db pid = case lookupInstalledPackageId db pid of
    Nothing -> return ()
    Just pkgi -> putStrLn $ "    " ++ nameOfPkgInfo pkgi
