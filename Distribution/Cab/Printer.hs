{-# LANGUAGE CPP #-}
module Distribution.Cab.Printer (
    printDeps
  , printRevDeps
  , extraInfo
  ) where

import Control.Monad
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Distribution.Cab.PkgDB
import Distribution.Cab.Version
import Distribution.Cab.Utils (UnitId, installedUnitId, lookupUnitId)
import Distribution.InstalledPackageInfo (author, depends, license)
import Distribution.License (License(..))
import Distribution.Simple.PackageIndex (allPackages)

#if MIN_VERSION_Cabal(2,2,0)
import Distribution.License (licenseFromSPDX)
#endif

----------------------------------------------------------------

type RevDB = Map UnitId [UnitId]

makeRevDepDB :: PkgDB -> RevDB
makeRevDepDB db = M.fromList revdeps
  where
    pkgs = allPackages db
    deps = map idDeps pkgs
    idDeps pkg = (installedUnitId pkg, depends pkg)
    kvs = sort $ concatMap decomp deps
    decomp (k,vs) = map (\v -> (v,k)) vs
    kvss = groupBy ((==) `on` fst) kvs
    comp xs = (fst (head xs), map snd xs)
    revdeps = map comp kvss

----------------------------------------------------------------

printDeps :: Bool -> Bool -> PkgDB -> Int -> PkgInfo -> IO ()
printDeps rec info db n pkgi = mapM_ (printDep rec info db n) $ depends pkgi

printDep :: Bool -> Bool -> PkgDB -> Int -> UnitId -> IO ()
printDep rec info db n uid = case lookupUnitId db uid of
    Nothing    -> return ()
    Just uniti -> do
        putStr $ prefix ++ fullNameOfPkgInfo uniti
        extraInfo info uniti
        putStrLn ""
        when rec $ printDeps rec info db (n+1) uniti
  where
    prefix = replicate (n * 4) ' '

----------------------------------------------------------------

printRevDeps :: Bool -> Bool -> PkgDB -> Int -> PkgInfo -> IO ()
printRevDeps rec info db n pkgi = printRevDeps' rec info db revdb n pkgi
  where
    revdb = makeRevDepDB db

printRevDeps' :: Bool -> Bool -> PkgDB -> RevDB -> Int -> PkgInfo -> IO ()
printRevDeps' rec info db revdb n pkgi = case M.lookup unitid revdb of
    Nothing -> return ()
    Just unitids -> mapM_ (printRevDep' rec info db revdb n) unitids
  where
    unitid = installedUnitId pkgi

printRevDep' :: Bool -> Bool -> PkgDB -> RevDB -> Int -> UnitId -> IO ()
printRevDep' rec info db revdb n uid = case lookupUnitId db uid of
    Nothing    -> return ()
    Just uniti -> do
        putStr $ prefix ++ fullNameOfPkgInfo uniti
        extraInfo info uniti
        putStrLn ""
        when rec $ printRevDeps' rec info db revdb (n+1) uniti
  where
    prefix = replicate (n * 4) ' '

----------------------------------------------------------------

extraInfo :: Bool -> PkgInfo -> IO ()
extraInfo False _ = return ()
extraInfo True pkgi = putStr $ " " ++ lcns ++ " \"" ++ show auth ++ "\""
  where
    lcns = showLicense (pkgInfoLicense pkgi)
    auth = author pkgi

pkgInfoLicense :: PkgInfo -> License
#if MIN_VERSION_Cabal(2,2,0)
pkgInfoLicense = either licenseFromSPDX id . license
#else
pkgInfoLicense = license
#endif

showLicense :: License -> String
showLicense (GPL (Just v))     = "GPL" ++ versionToString v
showLicense (GPL Nothing)      = "GPL"
showLicense (LGPL (Just v))    = "LGPL" ++ versionToString v
showLicense (LGPL Nothing)     = "LGPL"
showLicense (UnknownLicense s) = s
showLicense x                  = show x
