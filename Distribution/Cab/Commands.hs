module Distribution.Cab.Commands (
    deps, revdeps, installed, outdated, uninstall, search
  , genpaths, check, add, ghci
  ) where

import Control.Applicative hiding (many)
import Control.Monad
import Data.Char
import Data.List
import Distribution.Cab.GHCVer
import Distribution.Cab.GenPaths
import Distribution.Cab.PkgDB
import Distribution.Cab.Types
import Distribution.Cab.Utils
import Distribution.Cab.VerDB
import System.Exit
import System.IO
import System.Process hiding (env)

----------------------------------------------------------------

search :: FunctionCommand
search [x] _ = do
    nvls <- getVerAlist False
    forM_ (lok nvls) $ \(n,v) -> putStrLn $ n ++ " " ++ toDotted v
  where
    key = map toLower x
    sat (n,_) = key `isPrefixOf` map toLower n
    lok [] = []
    lok (e:es)
      | sat e   = e : lok es
      | otherwise = lok es
search _ _ = do
    hPutStrLn stderr "One search-key should be specified."
    exitFailure

----------------------------------------------------------------

installed :: FunctionCommand
installed _ opts = do
    let optall = OptAll `elem` opts
        optrec = OptRecursive `elem` opts
    db' <- getPkgDB (getSandbox opts)
    flt <- if optall then allPkgs else userPkgs
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
outdated _ opts = do
    flt <- if OptAll `elem` opts then allPkgs else userPkgs
    pkgs <- toPkgList flt <$> getPkgDB (getSandbox opts)
    verDB <- getVerDB
    forM_ pkgs $ \p -> case lookupLatestVersion (nameOfPkgInfo p) verDB of
        Nothing -> return ()
        Just ver -> when (numVersionOfPkgInfo p /= ver) $
                        putStrLn $ fullNameOfPkgInfo p ++ " < " ++ toDotted ver

----------------------------------------------------------------

uninstall :: FunctionCommand
uninstall nmver opts = do
    db' <- getPkgDB (getSandbox opts)
    db <- toPkgDB . flip toPkgList db' <$> userPkgs
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
        when doit $ void . system $ script pkgconf
      else
        putStrLn $ name ++ " " ++ ver
  where
    script pkgconf = "ghc-pkg unregister " ++ pkgconf ++ name ++ "-" ++ ver

pkgConfOpt :: [Option] -> IO String
pkgConfOpt opts = case getSandbox opts of
    Nothing   -> return ""
    Just path -> do
        ghcver <- ghcVersion
        pkgConf <- getPackageConf path
        let pkgOpt | ghcver >= 706 = "-package-db "
                   | otherwise     = "-package-conf "
        return $ pkgOpt ++ pkgConf ++ " "

----------------------------------------------------------------

genpaths :: FunctionCommand
genpaths _ _ = genPaths

----------------------------------------------------------------

check :: FunctionCommand
check _ opts = do
    pkgconf <- pkgConfOpt opts
    void . system $ script pkgconf
  where
    script pkgconf = "ghc-pkg check -v " ++ pkgconf

----------------------------------------------------------------

deps :: FunctionCommand
deps nmver opts = printDepends nmver opts printDeps

revdeps :: FunctionCommand
revdeps nmver opts = printDepends nmver opts printRevDeps

printDepends :: [String] -> [Option]
             -> (Bool -> Bool -> PkgDB -> Int -> PkgInfo -> IO ()) -> IO ()
printDepends nmver opts func = do
    db' <- getPkgDB (getSandbox opts)
    pkg <- lookupPkg nmver db'
    db <- if OptAll `elem` opts
          then return db'
          else toPkgDB . flip toPkgList db' <$> userPkgs
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

add :: FunctionCommand
add params opts = case getSandbox opts of
    Nothing -> hPutStrLn stderr "A sandbox must be specified with \"-s\" option."
    Just sbox -> case params of
        [src] -> void . system $ "cabal-dev add-source " ++ src ++ " -s " ++ sbox
        _     -> hPutStrLn stderr "A source path be specified."

----------------------------------------------------------------

ghci :: FunctionCommand
ghci _ opts = case getSandbox opts of
    Nothing -> hPutStrLn stderr "A sandbox must be specified with \"-s\" option."
    Just sbox -> do
      _ <- system $ "cabal-dev -s " ++ sbox ++ " ghci"
      return ()
