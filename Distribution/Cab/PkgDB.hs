module Distribution.Cab.PkgDB (
  -- * Types
    PkgDB
  , PkgInfo
  -- * Obtaining 'PkgDB'
  , getPkgDB
  , getGlobalPkgDB
  -- * From 'PkgInfo' to 'PkgDB'
  , toPkgDB
  -- * From 'PkgDB' to 'PkgInfo'
  , lookupByName
  , lookupByVersion
  , topSortedPkgs
  -- * From 'PkgDB' to 'PkgInfo'
  , allPkgs
  , userPkgs
  , toPkgList
  -- * From 'PkgInfo'
  , nameOfPkgInfo
  , fullNameOfPkgInfo
  , pairNameOfPkgInfo
  , verOfPkgInfo
  ) where

import Data.Maybe (isNothing)
import Distribution.Cab.Utils (fromDotted)
import Distribution.Cab.Version
import Distribution.Cab.VerDB (PkgName)
import Distribution.Compiler (CompilerId(..))
import Distribution.Version (Version(..))
import Distribution.InstalledPackageInfo
    (InstalledPackageInfo_(..), InstalledPackageInfo)
import Distribution.Package
    (PackageName(..), PackageIdentifier(..))
import Distribution.Simple.Compiler
    (PackageDB(..),Compiler(..))
import Distribution.Simple.GHC
    (configure, getInstalledPackages)
import Distribution.Simple.PackageIndex
    (lookupPackageName, lookupSourcePackageId, lookupInstalledPackageId
    , allPackages, fromList, reverseDependencyClosure
    , topologicalOrder, PackageIndex)
import Distribution.Simple.Program.Db
    (defaultProgramDb)
import Distribution.Verbosity
    (normal)
import System.FilePath


type PkgDB = PackageIndex
type PkgInfo = InstalledPackageInfo

----------------------------------------------------------------

getPkgDB :: Maybe FilePath -> IO PkgDB
getPkgDB mpath = do
    (com,pro) <- configure normal Nothing Nothing defaultProgramDb
    let userDB = case mpath of
            Nothing -> UserPackageDB
            Just path -> SpecificPackageDB $ packageConf path com
    getInstalledPackages normal [GlobalPackageDB,userDB] pro

getGlobalPkgDB :: IO PkgDB
getGlobalPkgDB = do
    (_,pro) <- configure normal Nothing Nothing defaultProgramDb
    getInstalledPackages normal [GlobalPackageDB] pro

packageConf :: FilePath -> Compiler -> FilePath
packageConf path com = path </> "packages-" ++ versionToString ver ++ ".conf"
  where
    CompilerId _ ver = compilerId com

toPkgDB :: [PkgInfo] -> PkgDB
toPkgDB = fromList

----------------------------------------------------------------

lookupByName :: String -> PkgDB -> [PkgInfo]
lookupByName name db = concatMap snd $ lookupPackageName db (PackageName name)

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

toPkgList :: (PkgInfo -> Bool) -> PkgDB -> [PkgInfo]
toPkgList prd db = filter prd $ allPackages db

userPkgs :: IO (PkgInfo -> Bool)
userPkgs = do
    gDB <- getGlobalPkgDB
    return $ \pkgi -> isNothing $ lookupInstalledPackageId gDB (installedPackageId pkgi)

allPkgs :: IO (PkgInfo -> Bool)
allPkgs = return (const True)

----------------------------------------------------------------

nameOfPkgInfo :: PkgInfo -> PkgName
nameOfPkgInfo = toString . pkgName . sourcePackageId
  where
    toString (PackageName x) = x

fullNameOfPkgInfo :: PkgInfo -> String
fullNameOfPkgInfo pkgi = nameOfPkgInfo pkgi ++ " " ++ verToString (verOfPkgInfo pkgi)

pairNameOfPkgInfo :: PkgInfo -> (PkgName,String)
pairNameOfPkgInfo pkgi = (nameOfPkgInfo pkgi, verToString (verOfPkgInfo pkgi))

verOfPkgInfo :: PkgInfo -> Ver
verOfPkgInfo = version . pkgVersion . sourcePackageId

----------------------------------------------------------------

topSortedPkgs :: PkgInfo -> PkgDB -> [PkgInfo]
topSortedPkgs pkgi db = topSort $ pkgids [pkgi]
  where
    pkgids = map installedPackageId
    topSort = topologicalOrder . fromList . reverseDependencyClosure db
