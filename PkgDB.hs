{-# LANGUAGE CPP #-}

module PkgDB where

import Control.Monad
import Data.List
import Data.Map (Map)
import Data.Maybe (isNothing)
import qualified Data.Map as M
import Distribution.Compiler
    (CompilerId(..))
import Distribution.License
    (License(..))
import Distribution.Version
    (Version(..))
import Distribution.InstalledPackageInfo
    (InstalledPackageInfo_(..), InstalledPackageInfo)
import Distribution.Package
    (PackageName(..), PackageIdentifier(..), InstalledPackageId)
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
import Utils

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

getPackageConf :: FilePath -> IO FilePath
getPackageConf path = do
    (com,_) <- configure normal Nothing Nothing defaultProgramDb
    return $ packageConf path com

packageConf :: FilePath -> Compiler -> FilePath
packageConf path com = path </> "packages-" ++ version ver ++ ".conf"
  where
    CompilerId _ ver = compilerId com

toPkgDB :: [PkgInfo] -> PkgDB
toPkgDB = fromList

version :: Version -> String
version = toDotted . versionBranch

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
    return$ \pkgi -> isNothing$ lookupInstalledPackageId gDB (installedPackageId pkgi)
    
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

----------------------------------------------------------------

extraInfo :: Bool -> PkgInfo -> IO ()
extraInfo False _ = return ()
extraInfo True pkgi = putStr $ " " ++ lcns ++ " \"" ++  auth ++ "\""
  where
    lcns = showLicense (license pkgi)
    auth = author pkgi

----------------------------------------------------------------

printDeps :: Bool -> Bool -> PkgDB -> Int -> PkgInfo -> IO ()
printDeps rec info db n pkgi = mapM_ (printDep rec info db n) $ depends pkgi

printDep :: Bool -> Bool -> PkgDB -> Int -> InstalledPackageId -> IO ()
printDep rec info db n pid = case lookupInstalledPackageId db pid of
    Nothing -> return ()
    Just pkgi -> do
        putStr $ prefix ++ fullNameOfPkgInfo pkgi
        extraInfo info pkgi
        putStrLn ""
        when rec $ printDeps rec info db (n+1) pkgi
  where
    prefix = replicate (n * 4) ' '

showLicense :: License -> String
showLicense (GPL (Just v))     = "GPL" ++ version v
showLicense (GPL Nothing)      = "GPL"
showLicense (LGPL (Just v))    = "LGPL" ++ version v
showLicense (LGPL Nothing)     = "LGPL"
showLicense (UnknownLicense s) = s
showLicense x                  = show x

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
    Nothing -> return ()
    Just pkgi -> do
        putStr $ prefix ++ fullNameOfPkgInfo pkgi
        extraInfo info pkgi
        putStrLn ""
        when rec $ printRevDeps' rec info db revdb (n+1) pkgi
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
