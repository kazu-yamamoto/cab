module Commands (
    deps, revdeps, installed, outdated, uninstall, search, env
  , genpaths, check, add, ghci
  ) where

import Control.Applicative hiding (many)
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import GenPaths
import PkgDB
import System.Exit
import System.IO
import System.Process hiding (env)
import Types
import Utils
import VerDB

----------------------------------------------------------------

search :: FunctionCommand
search _ [x] _ = do
    nvls <- getVerAlist False
    forM_ (lok nvls) $ \(n,v) -> putStrLn $ n ++ " " ++ toDotted v
  where
    key = map toLower x
    sat (n,_) = key `isPrefixOf` map toLower n
    lok [] = []
    lok (e:es)
      | sat e   = e : lok es
      | otherwise = lok es
search _ _ _ = do
    hPutStrLn stderr "One search-key should be specified."
    exitFailure

----------------------------------------------------------------

installed :: FunctionCommand
installed _ _ opts = do
    let optall = OptAll `elem` opts
        optrec = OptRecursive `elem` opts
    db' <- getPkgDB (getSandbox opts)
    flt <- if optall then allPkgs else userPkgs (getSandbox opts)
    -- FIXME: the optall case does unnecessary conversion
    let pkgs = toPkgList flt db'
        db = toPkgDB pkgs
    forM_ pkgs $ \pkgi -> do
        putStr $ fullNameOfPkgInfo pkgi
        extraInfo info pkgi
        putStrLn ""
        when optrec $ printDeps True info db 1 pkgi
  where
    info = OptInfo `elem` opts

outdated :: FunctionCommand
outdated _ _ opts = do
    flt <- if OptAll `elem` opts then allPkgs else userPkgs (getSandbox opts)
    pkgs <- toPkgList flt <$> getPkgDB (getSandbox opts)
    verDB <- getVerDB
    forM_ pkgs $ \p -> case lookupLatestVersion (nameOfPkgInfo p) verDB of
        Nothing -> return ()
        Just ver -> when (numVersionOfPkgInfo p /= ver) $
                        putStrLn $ fullNameOfPkgInfo p ++ " < " ++ toDotted ver

----------------------------------------------------------------

uninstall :: FunctionCommand
uninstall _ nmver opts = do
    db' <- getPkgDB (getSandbox opts)
    db <- toPkgDB . flip toPkgList db' <$> userPkgs (getSandbox opts)
    pkg <- lookupPkg nmver db
    let sortedPkgs = topSortedPkgs pkg db
    if onlyOne && length sortedPkgs /= 1 then do
        hPutStrLn stderr "The following packages depend on this. Use the \"-r\" option."
        mapM_ (hPutStrLn stderr . fullNameOfPkgInfo) (init sortedPkgs)
      else do
        unless doit $ putStrLn "The following packages are deleted without the \"-n\" option."
        mapM_ (unregister doit opts . pairNameOfPkgInfo) sortedPkgs
  where
    onlyOne = OptRecursive `notElem` opts
    doit = OptNoharm `notElem` opts

unregister :: Bool -> [Option] -> (String,String) -> IO ()
unregister doit opts (name,ver) =
    if doit then do
        putStrLn $ "Deleting " ++ name ++ " " ++ ver
        pkgconf <- pkgConfOpt opts
        when doit $ system (script pkgconf) >> return ()
      else
        putStrLn $ name ++ " " ++ ver
  where
    script pkgconf = "ghc-pkg unregister " ++ pkgconf ++ name ++ "-" ++ ver

pkgConfOpt :: [Option] -> IO String
pkgConfOpt opts = case getSandbox opts of
    Nothing -> return ""
    Just path -> do
        pkgConf <- getPackageConf path
        return $ "--package-conf=" ++ pkgConf ++ " "

----------------------------------------------------------------

genpaths :: FunctionCommand
genpaths _ _ _ = genPaths

----------------------------------------------------------------

check :: FunctionCommand
check _ _ opts = do
    pkgconf <- pkgConfOpt opts
    system (script pkgconf)
    return ()
  where
   script pkgconf = "ghc-pkg check -v " ++ pkgconf

----------------------------------------------------------------

deps :: FunctionCommand
deps _ nmver opts = printDepends nmver opts printDeps

revdeps :: FunctionCommand
revdeps _ nmver opts = printDepends nmver opts printRevDeps

printDepends :: [String] -> [Option]
             -> (Bool -> Bool -> PkgDB -> Int -> PkgInfo -> IO ()) -> IO ()
printDepends nmver opts func = do
    db' <- getPkgDB (getSandbox opts)
    pkg <- lookupPkg nmver db'
    db <- if OptAll `elem` opts
          then return db'
          else toPkgDB . flip toPkgList db' <$> userPkgs (getSandbox opts)
    func rec info db 0 pkg
  where
    rec = OptRecursive `elem` opts
    info = OptInfo `elem` opts

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
    mapM_ (hPutStrLn stderr . fullNameOfPkgInfo) pkgs
    exitFailure

----------------------------------------------------------------

env :: FunctionCommand
env _ _ opts = case getSandbox opts of
    Nothing -> do
        putStrLn "unset CAB_SANDBOX_PATH"
        putStrLn "unsetenv CAB_SANDBOX_PATH"
        putStrLn ""
        putStrLn "unset GHC_PACKAGE_PATH"
        putStrLn "unsetenv GHC_PACKAGE_PATH"
    Just path -> do
        pkgConf <- getPackageConf path
        gPkgConf <- globalPackageDB
        putStrLn $ "export CAB_SANDBOX_PATH=" ++ path
        putStrLn $ "setenv CAB_SANDBOX_PATH " ++ path
        putStrLn ""
        putStrLn "The following commands are not necessary in normal case."
        let confs = gPkgConf ++ ":" ++ pkgConf
        putStrLn $ "export GHC_PACKAGE_PATH=" ++ confs
        putStrLn $ "setenv GHC_PACKAGE_PATH " ++ confs

globalPackageDB :: IO String
globalPackageDB = do
    res <- readProcess "ghc" ["--info"] []
    let alist = read res :: [(String,String)]
    return . fromJust $ lookup "Global Package DB" alist

----------------------------------------------------------------

add :: FunctionCommand
add _ params opts = case getSandbox opts of
    Nothing -> hPutStrLn stderr "A sandbox must be specified with \"-s\" option."
    Just sbox -> case params of
        [src] -> do
            system $ "cabal-dev add-source " ++ src ++ " -s " ++ sbox
            return ()
        _ -> hPutStrLn stderr "A source path be specified."

----------------------------------------------------------------

ghci :: FunctionCommand
ghci _ _ opts = case getSandbox opts of
    Nothing -> hPutStrLn stderr "A sandbox must be specified with \"-s\" option."
    Just sbox -> do
      system $ "cabal-dev -s " ++ sbox ++ " ghci"
      return ()
