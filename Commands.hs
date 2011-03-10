module Commands (
    deps, revdeps, installed, outdated
  ) where

import Control.Applicative hiding (many)
import Control.Monad
import PkgDB
import Types
import VerDB
import Utils
import Distribution.Simple.PackageIndex
import Distribution.InstalledPackageInfo

deps :: FunctionCommand
deps _ [] _ = return () -- FIXME
deps _ (pkgnm:_) _ = do
    db <- getPkgDB
    mapM_ (flip printPkg db) $ lookupByName pkgnm db

revdeps :: FunctionCommand
revdeps _ [] _ = return () -- FIXME
revdeps _ (pkgnm:_) _ = do
    db <- getPkgDB
    let pinfos = map installedPackageId $ lookupByName pkgnm db
        pkgs = topologicalOrder $ fromList $ reverseDependencyClosure db pinfos
    mapM_ (putStrLn . nameOfPkgInfo) pkgs

installed :: FunctionCommand
installed _ _ flags = do
    flt <- if allFlag flags then return (const True) else makeUserOnly
    pkgs <- flip toPkgList flt <$> getPkgDB
    mapM_ putStrLn $ map nameOfPkgInfo pkgs

outdated :: FunctionCommand
outdated _ _ flags = do
    flt <- if allFlag flags then return (const True) else makeUserOnly
    pkgs <- flip toPkgList flt <$> getPkgDB
    verDB <- getVerDB
    forM_ pkgs $ \p -> do
        case lookupLatestVersion (idOfPkgInfo p) verDB of
            Nothing -> return ()
            Just ver -> if versionOfPkgInfo p /= ver
               then putStrLn $ nameOfPkgInfo p ++ " < " ++ toDotted ver
               else return ()
