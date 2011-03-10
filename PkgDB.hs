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
    (lookupPackageName, lookupSourcePackageId, lookupInstalledPackageId
    ,allPackages, PackageIndex)
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
lookupByName name db = map (head . snd) $ lookupPackageName db (PackageName name)

lookupByVersion :: String -> String -> PkgDB -> [PkgInfo]
lookupByVersion name ver db = lookupSourcePackageId db src
  where
    src = PackageIdentifier {
        pkgName = PackageName name
      , pkgVersion = Version {
          versionBranch = fromDotted ver
        , versionTags = []
        }
      }

----------------------------------------------------------------

toPkgList :: PkgDB -> (PkgInfo -> Bool) -> [PkgInfo]
toPkgList db prd = filter prd $ allPackages db

userPkgs :: IO (PkgInfo -> Bool)
userPkgs = do
    userDirPref <- getAppUserDataDirectory ""
    return $ \pkgi -> userDirPref `isPrefixOf` (head $ libraryDirs pkgi)

allPkgs :: IO (PkgInfo -> Bool)
allPkgs = return (const True)

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
