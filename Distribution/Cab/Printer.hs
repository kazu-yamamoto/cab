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
import Distribution.InstalledPackageInfo (InstalledPackageInfo_(..))
import Distribution.License (License(..))
import Distribution.Package (InstalledPackageId)
import Distribution.Simple.PackageIndex (lookupInstalledPackageId, allPackages)

----------------------------------------------------------------

type RevDB = Map InstalledPackageId [InstalledPackageId]

makeRevDepDB :: PkgDB -> RevDB
makeRevDepDB db = M.fromList revdeps
  where
    pkgs = allPackages db
    deps = map idDeps pkgs
    idDeps pkg = (installedPackageId pkg, depends pkg)
    kvs = sort $ concatMap decomp deps
    decomp (k,vs) = map (\v -> (v,k)) vs
    kvss = groupBy ((==) `on` fst) kvs
    comp xs = (fst (head xs), map snd xs)
    revdeps = map comp kvss

----------------------------------------------------------------

printDeps :: Bool -> Bool -> PkgDB -> Int -> PkgInfo -> IO ()
printDeps rec info db n pkgi = mapM_ (printDep rec info db n) $ depends pkgi

printDep :: Bool -> Bool -> PkgDB -> Int -> InstalledPackageId -> IO ()
printDep rec info db n pid = case lookupInstalledPackageId db pid of
    Nothing   -> return ()
    Just pkgi -> do
        putStr $ prefix ++ fullNameOfPkgInfo pkgi
        extraInfo info pkgi
        putStrLn ""
        when rec $ printDeps rec info db (n+1) pkgi
  where
    prefix = replicate (n * 4) ' '

----------------------------------------------------------------

printRevDeps :: Bool -> Bool -> PkgDB -> Int -> PkgInfo -> IO ()
printRevDeps rec info db n pkgi = printRevDeps' rec info db revdb n pkgi
  where
    revdb = makeRevDepDB db

printRevDeps' :: Bool -> Bool -> PkgDB -> RevDB -> Int -> PkgInfo -> IO ()
printRevDeps' rec info db revdb n pkgi = case M.lookup pkgid revdb of
    Nothing -> return ()
    Just pkgids -> mapM_ (printRevDep' rec info db revdb n) pkgids
  where
    pkgid = installedPackageId pkgi

printRevDep' :: Bool -> Bool -> PkgDB -> RevDB -> Int -> InstalledPackageId -> IO ()
printRevDep' rec info db revdb n pid = case lookupInstalledPackageId db pid of
    Nothing   -> return ()
    Just pkgi -> do
        putStr $ prefix ++ fullNameOfPkgInfo pkgi
        extraInfo info pkgi
        putStrLn ""
        when rec $ printRevDeps' rec info db revdb (n+1) pkgi
  where
    prefix = replicate (n * 4) ' '

----------------------------------------------------------------

extraInfo :: Bool -> PkgInfo -> IO ()
extraInfo False _ = return ()
extraInfo True pkgi = putStr $ " " ++ lcns ++ " \"" ++  auth ++ "\""
  where
    lcns = showLicense (license pkgi)
    auth = author pkgi

showLicense :: License -> String
showLicense (GPL (Just v))     = "GPL" ++ versionToString v
showLicense (GPL Nothing)      = "GPL"
showLicense (LGPL (Just v))    = "LGPL" ++ versionToString v
showLicense (LGPL Nothing)     = "LGPL"
showLicense (UnknownLicense s) = s
showLicense x                  = show x
