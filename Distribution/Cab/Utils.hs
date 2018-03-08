{-# LANGUAGE CPP #-}
module Distribution.Cab.Utils where

import Data.List

import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.Package (PackageName)
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Verbosity (Verbosity)

#if MIN_VERSION_Cabal(1,21,0) && !(MIN_VERSION_Cabal(1,23,0))
import Distribution.Package (PackageInstalled)
#endif

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

#if MIN_VERSION_Cabal(2,0,0)
import qualified Distribution.Package as Cabal
    (mkPackageName, unPackageName)
#else
import qualified Distribution.Package as Cabal (PackageName(..))
#endif

#if MIN_VERSION_Cabal(2,2,0)
import qualified Distribution.PackageDescription.Parsec as Cabal
    (readGenericPackageDescription)
#elif MIN_VERSION_Cabal(2,0,0)
import qualified Distribution.PackageDescription.Parse as Cabal
    (readGenericPackageDescription)
#else
import qualified Distribution.PackageDescription.Parse as Cabal
    (readPackageDescription)
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

-- UnitIds

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

-- PackageNames

mkPackageName :: String -> PackageName
#if MIN_VERSION_Cabal(2,0,0)
mkPackageName = Cabal.mkPackageName
#else
mkPackageName = Cabal.PackageName
#endif

unPackageName :: PackageName -> String
#if MIN_VERSION_Cabal(2,0,0)
unPackageName = Cabal.unPackageName
#else
unPackageName (Cabal.PackageName s) = s
#endif

-- GenericPackageDescription

readGenericPackageDescription :: Verbosity -> FilePath -> IO GenericPackageDescription
#if MIN_VERSION_Cabal(2,0,0)
readGenericPackageDescription = Cabal.readGenericPackageDescription
#else
readGenericPackageDescription = Cabal.readPackageDescription
#endif
