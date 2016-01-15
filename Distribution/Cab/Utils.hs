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
    (installedComponentId)
import Distribution.Package (ComponentId)
import qualified Distribution.Simple.PackageIndex as Cabal
    (lookupComponentId)
#else
import qualified Distribution.InstalledPackageInfo as Cabal
    (installedPackageId)
import Distribution.Package (InstalledPackageId)
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
installedComponentId :: InstalledPackageInfo -> ComponentId
installedComponentId = Cabal.installedComponentId
#else
installedComponentId :: InstalledPackageInfo -> InstalledPackageId
installedComponentId = Cabal.installedPackageId
#endif

#if MIN_VERSION_Cabal(1,23,0)
lookupComponentId :: PackageIndex a -> ComponentId -> Maybe a
lookupComponentId = Cabal.lookupComponentId
#elif MIN_VERSION_Cabal(1,21,0)
lookupComponentId :: PackageInstalled a => PackageIndex a -> InstalledPackageId -> Maybe a
lookupComponentId = Cabal.lookupInstalledPackageId
#else
lookupComponentId :: PackageIndex -> InstalledPackageId
lookupComponentId = Cabal.lookupInstalledPackageId
#endif
