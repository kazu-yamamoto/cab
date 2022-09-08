{-# LANGUAGE CPP #-}
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
  -- * Find internal libraries
  , findInternalLibs
  ) where

import Distribution.Cab.Utils
    (fromDotted, installedUnitId, mkPackageName, unPackageName)
import Distribution.Cab.Version
import Distribution.Cab.VerDB (PkgName)
import Distribution.InstalledPackageInfo
    (InstalledPackageInfo(depends), sourcePackageId)
import Distribution.Package (PackageIdentifier(..))
import Distribution.Simple.Compiler (PackageDB(..))
import Distribution.Simple.GHC (configure, getInstalledPackages, getPackageDBContents)
import Distribution.Simple.PackageIndex
    (lookupPackageName, lookupSourcePackageId, allPackages
    , fromList, reverseDependencyClosure, topologicalOrder)
#if MIN_VERSION_Cabal(1,22,0)
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
#else
import Distribution.Simple.PackageIndex (PackageIndex)
#endif
import Distribution.Simple.Program.Db (defaultProgramDb)
import Distribution.Verbosity (normal)
import Distribution.Types.UnitId (unUnitId)

import Data.Char
import Data.Maybe

----------------------------------------------------------------

#if MIN_VERSION_Cabal(1,22,0)
type PkgDB = InstalledPackageIndex
#else
type PkgDB = PackageIndex
#endif
type PkgInfo = InstalledPackageInfo

----------------------------------------------------------------

-- | Obtaining 'PkgDB' for global and user
--
-- > getSandbox >>= getPkgDB
getPkgDB :: Maybe FilePath -> IO PkgDB
getPkgDB mpath = getDBs [GlobalPackageDB,userDB]
  where
    userDB = toUserSpec mpath

-- | Obtaining 'PkgDB' for user
getUserPkgDB :: Maybe FilePath -> IO PkgDB
getUserPkgDB mpath = getDB userDB
  where
    userDB = toUserSpec mpath

-- | Obtaining 'PkgDB' for global
getGlobalPkgDB :: IO PkgDB
getGlobalPkgDB = getDB GlobalPackageDB

toUserSpec :: Maybe FilePath -> PackageDB
toUserSpec Nothing     = UserPackageDB
toUserSpec (Just path) = SpecificPackageDB path

getDBs :: [PackageDB] -> IO PkgDB
getDBs specs = do
    (_comp,_,pro) <- configure normal Nothing Nothing defaultProgramDb
    getInstalledPackages normal
#if MIN_VERSION_Cabal(1,23,0)
                         _comp
#endif
                         specs pro

getDB :: PackageDB -> IO PkgDB
getDB spec = do
    (_,_,pro) <- configure normal Nothing Nothing defaultProgramDb
    getPackageDBContents normal spec pro

----------------------------------------------------------------

-- |
--
-- > pkgdb <- getGlobalPkgDB
-- > lookupByName "base" pkgdb
lookupByName :: PkgName -> PkgDB -> [PkgInfo]
lookupByName name db = concatMap snd $ lookupPackageName db (mkPackageName name)

-- |
--
-- > pkgdb <- getGlobalPkgDB
-- > lookupByVersion "base" "4.6.0.1" pkgdb
lookupByVersion :: PkgName -> String -> PkgDB -> [PkgInfo]
lookupByVersion name ver db = lookupSourcePackageId db src
  where
    src = PackageIdentifier {
        pkgName = mkPackageName name
      , pkgVersion = toVersion $ fromDotted ver
      }

----------------------------------------------------------------

toPkgInfos :: PkgDB -> [PkgInfo]
toPkgInfos db = allPackages db

----------------------------------------------------------------

nameOfPkgInfo :: PkgInfo -> PkgName
nameOfPkgInfo = unPackageName . pkgName . sourcePackageId

fullNameOfPkgInfo :: PkgInfo -> String
fullNameOfPkgInfo pkgi = nameOfPkgInfo pkgi ++ " " ++ verToString (verOfPkgInfo pkgi)

pairNameOfPkgInfo :: PkgInfo -> (PkgName,String)
pairNameOfPkgInfo pkgi = (nameOfPkgInfo pkgi, verToString (verOfPkgInfo pkgi))

verOfPkgInfo :: PkgInfo -> Ver
verOfPkgInfo = version . pkgVersion . sourcePackageId

----------------------------------------------------------------

topSortedPkgs :: PkgInfo -> PkgDB -> [PkgInfo]
topSortedPkgs pkgi db = topSort $ unitids [pkgi]
  where
    unitids = map installedUnitId
    topSort = topologicalOrder . fromList . reverseDependencyClosure db

----------------------------------------------------------------

findInternalLibs :: PkgInfo -> [String]
findInternalLibs pkgInfo =
    catMaybes $ map (getInternalLib . unUnitId) $ depends pkgInfo

getInternalLib :: String -> Maybe String
getInternalLib xs0 = case drop 22 $ skip xs0 of
  _:xs1   -> Just $ take (length xs1) xs1
  _       -> Nothing
  where
    skip ys = case break (== '-') ys of
      (_,'-':b:bs)
        | isDigit b -> case break (== '-') bs of
            (_,'-':ds) -> ds
            _          -> "" -- error
        | otherwise -> skip bs
      _  -> "" -- error
