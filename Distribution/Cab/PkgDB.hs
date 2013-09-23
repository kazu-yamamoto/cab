module Distribution.Cab.PkgDB (
  -- * Types
    PkgDB
  , PkgInfo
  -- * Obtaining 'PkgDB'
  , getPkgDB
  , getGlobalPkgDB
  , getUserPkgDB
  -- * Looking up
  , lookupByName
  , lookupByVersion
  -- * Topological sorting
  , topSortedPkgs
  -- * To 'PkgInfo'
  , toPkgInfos
  -- * From 'PkgInfo'
  , nameOfPkgInfo
  , fullNameOfPkgInfo
  , pairNameOfPkgInfo
  , verOfPkgInfo
  ) where

import Distribution.Cab.Utils (fromDotted)
import Distribution.Cab.Version
import Distribution.Cab.VerDB (PkgName)
import Distribution.Compiler (CompilerId(..))
import Distribution.Version (Version(..))
import Distribution.InstalledPackageInfo
    (InstalledPackageInfo_(..), InstalledPackageInfo)
import Distribution.Package (PackageName(..), PackageIdentifier(..))
import Distribution.Simple.Compiler (PackageDB(..),Compiler(..))
import Distribution.Simple.GHC (configure, getInstalledPackages)
import Distribution.Simple.PackageIndex
    (lookupPackageName, lookupSourcePackageId
    , allPackages, fromList, reverseDependencyClosure
    , topologicalOrder, PackageIndex)
import Distribution.Simple.Program (ProgramConfiguration)
import Distribution.Simple.Program.Db (defaultProgramDb)
import Distribution.Verbosity (normal)
import System.FilePath


type PkgDB = PackageIndex
type PkgInfo = InstalledPackageInfo

----------------------------------------------------------------

-- | Obtaining 'PkgDB' for global and user
--
-- > getSandbox >>= getPkgDB
getPkgDB :: Maybe FilePath -> IO PkgDB
getPkgDB mpath = do
    (userDB,pro) <- getUserDB mpath
    getDB [GlobalPackageDB,userDB] pro

-- | Obtaining 'PkgDB' for user
getUserPkgDB :: Maybe FilePath -> IO PkgDB
getUserPkgDB mpath = do
    (userDB,pro) <- getUserDB mpath
    getDB [userDB] pro

getUserDB :: Maybe FilePath -> IO (PackageDB, ProgramConfiguration)
getUserDB mpath = do
    (com,pro) <- configure normal Nothing Nothing defaultProgramDb
    let userDB = case mpath of
            Nothing -> UserPackageDB
            Just path -> SpecificPackageDB $ packageConf path com
    return (userDB, pro)

-- | Obtaining 'PkgDB' for global
getGlobalPkgDB :: IO PkgDB
getGlobalPkgDB = do
    (_,pro) <- configure normal Nothing Nothing defaultProgramDb
    getDB [GlobalPackageDB] pro

getDB :: [PackageDB] -> ProgramConfiguration -> IO PackageIndex
getDB spec pro = getInstalledPackages normal spec pro

packageConf :: FilePath -> Compiler -> FilePath
packageConf path com = path </> "packages-" ++ versionToString ver ++ ".conf"
  where
    CompilerId _ ver = compilerId com

----------------------------------------------------------------

-- |
--
-- > pkgdb <- getGlobalPkgDB
-- > lookupByName "base" pkgdb
lookupByName :: PkgName -> PkgDB -> [PkgInfo]
lookupByName name db = concatMap snd $ lookupPackageName db (PackageName name)

-- |
--
-- > pkgdb <- getGlobalPkgDB
-- > lookupByVersion "base" "4.6.0.1" pkgdb
lookupByVersion :: PkgName -> String -> PkgDB -> [PkgInfo]
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

toPkgInfos :: PkgDB -> [PkgInfo]
toPkgInfos db = allPackages db

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
