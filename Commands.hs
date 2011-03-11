module Commands (
    deps, revdeps, installed, outdated, uninstall, search
  ) where

import Control.Applicative hiding (many)
import Control.Monad
import PkgDB
import System.Exit
import System.IO
import Types
import Utils
import VerDB
import System.Process
import Data.List
import Data.Char

----------------------------------------------------------------

search :: FunctionCommand
search _ [x] _ = do
    nvls <- getVerAlist False
    forM_ (lok nvls) $ \(n,v) -> putStrLn $ n ++ " " ++ toDotted v
  where
    key = map toLower x
    check (n,_) = key `isPrefixOf` map toLower n
    lok [] = []
    lok (e:es)
      | check e   = e : lok es
      | otherwise = lok es
search _ _ _ = do
    hPutStrLn stderr "One search-key should be specified."
    exitFailure

----------------------------------------------------------------

installed :: FunctionCommand
installed _ _ flags = do
    flt <- if allFlag flags then allPkgs else userPkgs
    pkgs <- toPkgList flt <$> getPkgDB
    mapM_ putStrLn $ map fullNameOfPkgInfo pkgs

outdated :: FunctionCommand
outdated _ _ flags = do
    flt <- if allFlag flags then allPkgs else userPkgs
    pkgs <- toPkgList flt <$> getPkgDB
    verDB <- getVerDB
    forM_ pkgs $ \p -> do
        case lookupLatestVersion (nameOfPkgInfo p) verDB of
            Nothing -> return ()
            Just ver -> if numVersionOfPkgInfo p /= ver
               then putStrLn $ fullNameOfPkgInfo p ++ " < " ++ toDotted ver
               else return ()

----------------------------------------------------------------

uninstall :: FunctionCommand
uninstall _ nmver flags = do
    db' <- getPkgDB
    db <- toPkgDB . flip toPkgList db' <$> userPkgs
    pkg <- lookupPkg nmver db
    let sortedPkgs = topSortedPkgs pkg db
    if onlyOne && length sortedPkgs /= 1
       then do
        hPutStrLn stderr $ "The following packages depend on this. Use the \"-r\" option."
        mapM_ (hPutStrLn stderr . fullNameOfPkgInfo) (init sortedPkgs)
       else do
        unless doit $ putStrLn "The following packages are deleted without the \"-n\" option."
        mapM_ (unregister doit) (map pairNameOfPkgInfo sortedPkgs)
  where
    onlyOne = not $ recursiveFlag flags
    doit = not $ noHarmFlag flags

unregister :: Bool -> (String,String) -> IO ()
unregister doit (name,ver) = if doit
    then do
        putStrLn $ "Deleting " ++ name ++ " " ++ ver
        when doit $ system script >> return ()
    else putStrLn $ name ++ " " ++ ver
  where
    script = "ghc-pkg unregister " ++ name ++ "-" ++ ver

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
    mapM_ (hPutStrLn stderr) $ map fullNameOfPkgInfo pkgs
    exitFailure
