module Commands (
    deps, revdeps, installed, outdated
  ) where

import Control.Applicative hiding (many)
import Control.Monad
import PkgDB
import System.Exit
import System.IO
import Types
import Utils
import VerDB

----------------------------------------------------------------

installed :: FunctionCommand
installed _ _ flags = do
    flt <- if allFlag flags then allPkgs else userPkgs
    pkgs <- toPkgList flt <$> getPkgDB
    mapM_ putStrLn $ map nameOfPkgInfo pkgs

outdated :: FunctionCommand
outdated _ _ flags = do
    flt <- if allFlag flags then allPkgs else userPkgs
    pkgs <- toPkgList flt <$> getPkgDB
    verDB <- getVerDB
    forM_ pkgs $ \p -> do
        case lookupLatestVersion (idOfPkgInfo p) verDB of
            Nothing -> return ()
            Just ver -> if versionOfPkgInfo p /= ver
               then putStrLn $ nameOfPkgInfo p ++ " < " ++ toDotted ver
               else return ()

----------------------------------------------------------------

deps :: FunctionCommand
deps _ nmver flags = printDepends nmver flags printDeps

revdeps :: FunctionCommand
revdeps _ nmver flags = printDepends nmver flags printRevDeps

printDepends :: [String] -> Flags
             -> (Bool -> PkgDB -> Int -> PkgInfo -> IO ()) -> IO ()
printDepends nmver flags func = do
    db' <- getPkgDB
    db <- if allFlag flags
          then return db'
          else toPkgDB . flip toPkgList db' <$> userPkgs
    pkg <- lookupPkg nmver db
    func (recursiveFlag flags) db 0 pkg

----------------------------------------------------------------

lookupPkg :: [String] -> PkgDB -> IO PkgInfo
lookupPkg [] _ = do
  hPutStrLn stderr "Package name must be specified."
  exitFailure
lookupPkg [name] db = checkOne $ lookupByName name db
lookupPkg [name,ver] db = checkOne $ lookupByVersion name ver db
lookupPkg _ _ = do
  hPutStrLn stderr "Only one package name must be specified."
  exitFailure

checkOne :: [PkgInfo] -> IO PkgInfo
checkOne [] = do
    hPutStrLn stderr "No such package found."
    exitFailure
checkOne [pkg] = return pkg
checkOne pkgs = do
    hPutStrLn stderr "Package version must be specified."
    mapM_ (hPutStrLn stderr) $ map nameOfPkgInfo pkgs
    exitFailure
