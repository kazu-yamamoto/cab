module Commands (
    deps, installed, outdated
  ) where

import Control.Applicative hiding (many)
import Control.Monad
import PkgDB
import Types
import VerDB
import Utils

deps :: FunctionCommand
deps _ [] _ = return () -- FIXME
deps _ (pkgnm:_) _ = do
    db <- getPkgDB
    mapM_ (flip printPkg db) $ lookupByName pkgnm db

installed :: FunctionCommand
installed _ _ _ = do
    pkgs <- toPkgList <$> getPkgDB <*> makeUserOnly
    mapM_ putStrLn $ map nameOfPkgInfo pkgs

outdated :: FunctionCommand
outdated _ _ _ = do
    pkgs <- toPkgList <$> getPkgDB <*> makeUserOnly
    verDB <- getVerDB
    forM_ pkgs $ \p -> do
        case lookupLatestVersion (idOfPkgInfo p) verDB of
            Nothing -> return ()
            Just ver -> if versionOfPkgInfo p /= ver
               then putStrLn $ nameOfPkgInfo p ++ " < " ++ toDotted ver
               else return ()
