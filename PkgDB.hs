module PkgDB where

import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
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
    , allPackages, fromList, reverseDependencyClosure
    , topologicalOrder, PackageIndex)
import Distribution.Simple.Program.Db
    (defaultProgramDb)
import Distribution.Verbosity
    (normal)
import System.Directory
import Utils

type PkgDB = PackageIndex
type PkgInfo = InstalledPackageInfo

----------------------------------------------------------------

getPkgDB :: IO PkgDB
getPkgDB = do
    (_,pro) <- configure normal Nothing Nothing defaultProgramDb
    getInstalledPackages normal [GlobalPackageDB,UserPackageDB] pro

toPkgDB :: [PkgInfo] -> PkgDB
toPkgDB = fromList

----------------------------------------------------------------

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

toPkgList :: (PkgInfo -> Bool) -> PkgDB -> [PkgInfo]
toPkgList prd db = filter prd $ allPackages db

userPkgs :: IO (PkgInfo -> Bool)
userPkgs = do
    userDirPref <- getAppUserDataDirectory ""
    return $ \pkgi -> userDirPref `isPrefixOf` (head $ libraryDirs pkgi)

allPkgs :: IO (PkgInfo -> Bool)
allPkgs = return (const True)

----------------------------------------------------------------

fullNameOfPkgInfo :: PkgInfo -> String
fullNameOfPkgInfo pkgi = nameOfPkgInfo pkgi ++ " " ++ versionOfPkgInfo pkgi

pairNameOfPkgInfo :: PkgInfo -> (String,String)
pairNameOfPkgInfo pkgi = (nameOfPkgInfo pkgi, versionOfPkgInfo pkgi)

nameOfPkgInfo :: PkgInfo -> String
nameOfPkgInfo = toString . pkgName . sourcePackageId
  where
    toString (PackageName x) = x

versionOfPkgInfo :: PkgInfo -> String
versionOfPkgInfo = toDotted . numVersionOfPkgInfo

numVersionOfPkgInfo :: PkgInfo -> [Int]
numVersionOfPkgInfo = versionBranch . pkgVersion . sourcePackageId

idOfPkgInfo :: PkgInfo -> PackageId
idOfPkgInfo = sourcePackageId

----------------------------------------------------------------

printDeps :: Bool -> PkgDB -> Int -> PkgInfo -> IO ()
printDeps rec db n pkgi = mapM_ (printDep rec db n) $ depends pkgi

printDep :: Bool -> PkgDB -> Int -> InstalledPackageId -> IO ()
printDep rec db n pid = case lookupInstalledPackageId db pid of
    Nothing -> return ()
    Just pkgi -> do
        putStrLn $ prefix ++ fullNameOfPkgInfo pkgi
        when rec $ printDeps rec db (n+1) pkgi
  where
    prefix = replicate (n * 4) ' '

----------------------------------------------------------------

printRevDeps :: Bool -> PkgDB -> Int -> PkgInfo -> IO ()
printRevDeps rec db n pkgi = printRevDeps' rec db revdb n pkgi
  where
    revdb = makeRevDepDB db

printRevDeps' :: Bool -> PkgDB -> RevDB -> Int -> PkgInfo -> IO ()
printRevDeps' rec db revdb n pkgi = case M.lookup pkgid revdb of
    Nothing -> return ()
    Just pkgids -> mapM_ (printRevDep' rec db revdb n) $ pkgids
  where
    pkgid = installedPackageId pkgi

printRevDep' :: Bool -> PkgDB -> RevDB -> Int -> InstalledPackageId -> IO ()
printRevDep' rec db revdb n pid = case lookupInstalledPackageId db pid of
    Nothing -> return ()
    Just pkgi -> do
        putStrLn $ prefix ++ fullNameOfPkgInfo pkgi
        when rec $ printRevDeps' rec db revdb (n+1) pkgi
  where
    prefix = replicate (n * 4) ' '

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
    kvss = groupBy (\x y -> fst x == fst y) kvs
    comp xs = (fst (head xs), map snd xs)
    revdeps = map comp kvss

----------------------------------------------------------------

topSortedPkgs :: PkgInfo -> PkgDB -> [PkgInfo]
topSortedPkgs pkgi db = topSort $ pkgids [pkgi]
  where
    pkgids = map installedPackageId
    topSort = topologicalOrder . fromList . reverseDependencyClosure db
