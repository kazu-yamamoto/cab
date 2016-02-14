{-# LANGUAGE CPP #-}
module Distribution.Cab.Utils where

import Data.List

import Distribution.InstalledPackageInfo (InstalledPackageInfo)
#if MIN_VERSION_Cabal(1,21,0) && !(MIN_VERSION_Cabal(1,23,0))
import Distribution.Package (PackageInstalled)
#endif
import Distribution.Simple.PackageIndex (PackageIndex)
#if MIN_VERSION_Cabal(1,23,0)
import qualified Distribution.InstalledPackageInfo as Cabal
    (installedUnitId)
import qualified Distribution.Package as Cabal (UnitId)
import qualified Distribution.Simple.PackageIndex as Cabal
    (lookupUnitId)
#else
import qualified Distribution.InstalledPackageInfo as Cabal
    (installedPackageId)
import qualified Distribution.Package as Cabal (InstalledPackageId)
import qualified Distribution.Simple.PackageIndex as Cabal
    (lookupInstalledPackageId)
#endif

-- |
-- >>> fromDotted "1.2.3"
-- [1,2,3]
fromDotted :: String -> [Int]
fromDotted [] = []
fromDotted xs = case break (=='.') xs of
    (x,"") -> [read x :: Int]
    (x,_:ys) -> (read x :: Int) : fromDotted ys

-- |
-- >>> toDotted [1,2,3]
-- "1.2.3"
toDotted :: [Int] -> String
toDotted = intercalate "." . map show

#if MIN_VERSION_Cabal(1,23,0)
type UnitId = Cabal.UnitId
#else
type UnitId = Cabal.InstalledPackageId
#endif

installedUnitId :: InstalledPackageInfo -> UnitId
#if MIN_VERSION_Cabal(1,23,0)
installedUnitId = Cabal.installedUnitId
#else
installedUnitId = Cabal.installedPackageId
#endif

#if MIN_VERSION_Cabal(1,23,0)
lookupUnitId :: PackageIndex a -> UnitId -> Maybe a
lookupUnitId = Cabal.lookupUnitId
#elif MIN_VERSION_Cabal(1,21,0)
lookupUnitId :: PackageInstalled a => PackageIndex a -> UnitId -> Maybe a
lookupUnitId = Cabal.lookupInstalledPackageId
#else
lookupUnitId :: PackageIndex -> UnitId -> Maybe InstalledPackageInfo
lookupUnitId = Cabal.lookupInstalledPackageId
#endif
