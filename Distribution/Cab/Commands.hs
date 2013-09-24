module Distribution.Cab.Commands (
    FunctionCommand
  , Option(..)
  , deps, revdeps, installed, outdated, uninstall, search
  , genpaths, check, initSandbox, add, ghci
  ) where

import Control.Applicative hiding (many)
import Control.Monad
import Data.Char
import Data.List (isPrefixOf, intercalate)
import qualified Data.Map as M
import Distribution.Cab.GenPaths
import Distribution.Cab.PkgDB
import Distribution.Cab.Printer
import Distribution.Cab.Sandbox
import Distribution.Cab.VerDB
import Distribution.Cab.Version
import System.Exit
import System.IO
import System.Process hiding (env)

----------------------------------------------------------------

type FunctionCommand = [String] -> [Option] -> IO ()

data Option = OptNoharm
            | OptRecursive
            | OptAll
            | OptInfo
            | OptFlag String
            | OptTest
            | OptHelp
            | OptBench
            | OptDepsOnly
            | OptLibProfile
            | OptExecProfile
            | OptJobs String
            deriving (Eq,Show)

----------------------------------------------------------------

search :: FunctionCommand
search [x] _ = do
    nvls <- toList <$> getVerDB AllRegistered
    forM_ (lok nvls) $ \(n,v) -> putStrLn $ n ++ " " ++ verToString v
  where
    key = map toLower x
    sat (n,_) = key `isPrefixOf` map toLower n
    lok = filter sat
search _ _ = do
    hPutStrLn stderr "One search-key should be specified."
    exitFailure

----------------------------------------------------------------

installed :: FunctionCommand
installed _ opts = do
    db <- getDB opts
    let pkgs = toPkgInfos db
    forM_ pkgs $ \pkgi -> do
        putStr $ fullNameOfPkgInfo pkgi
        extraInfo info pkgi
        putStrLn ""
        when optrec $ printDeps True info db 1 pkgi
  where
    info = OptInfo `elem` opts
    optrec = OptRecursive `elem` opts

outdated :: FunctionCommand
outdated _ opts = do
    pkgs <- toPkgInfos <$> getDB opts
    verDB <- toMap <$> getVerDB InstalledOnly
    forM_ pkgs $ \p -> case M.lookup (nameOfPkgInfo p) verDB of
        Nothing  -> return ()
        Just ver -> when (verOfPkgInfo p /= ver) $
                      putStrLn $ fullNameOfPkgInfo p ++ " < " ++ verToString ver

getDB :: [Option] -> IO PkgDB
getDB opts
  | optall    = getSandbox >>= getPkgDB
  | otherwise = getSandbox >>= getUserPkgDB
  where
    optall = OptAll `elem` opts

----------------------------------------------------------------

uninstall :: FunctionCommand
uninstall nmver opts = do
    userDB <- getSandbox >>= getUserPkgDB
    pkg <- lookupPkg nmver userDB
    let sortedPkgs = topSortedPkgs pkg userDB
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
unregister doit _ (name,ver) =
    if doit then do
        putStrLn $ "Deleting " ++ name ++ " " ++ ver
        sandboxOpts <- getSandboxOpts <$> getSandbox
        when doit $ void . system $ script sandboxOpts
      else
        putStrLn $ name ++ " " ++ ver
  where
    script sandboxOpts = "ghc-pkg unregister " ++ sandboxOpts ++ " " ++ name ++ "-" ++ ver

----------------------------------------------------------------

genpaths :: FunctionCommand
genpaths _ _ = genPaths

----------------------------------------------------------------

check :: FunctionCommand
check _ _ = do
    sandboxOpts <- getSandboxOpts <$> getSandbox
    void . system $ script sandboxOpts
  where
    script sandboxOpts = "ghc-pkg check -v " ++ sandboxOpts

----------------------------------------------------------------

deps :: FunctionCommand
deps nmver opts = printDepends nmver opts printDeps

revdeps :: FunctionCommand
revdeps nmver opts = printDepends nmver opts printRevDeps

printDepends :: [String] -> [Option]
             -> (Bool -> Bool -> PkgDB -> Int -> PkgInfo -> IO ()) -> IO ()
printDepends nmver opts func = do
    db' <- getSandbox >>= getPkgDB
    pkg <- lookupPkg nmver db'
    db <- getDB opts
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

initSandbox :: FunctionCommand
initSandbox []     _ = void . system $ "cabal sandbox init"
initSandbox [path] _ = void . system $ "cabal sandbox init --sandbox " ++ path
initSandbox _      _ = do
    hPutStrLn stderr "Only one argument is allowed"
    exitFailure

----------------------------------------------------------------

add :: FunctionCommand
add [src] _ = void . system $ "cabal sandbox add-source " ++ src
add _     _ = do
    hPutStrLn stderr "A source path be specified."
    exitFailure

----------------------------------------------------------------

ghci :: FunctionCommand
ghci args _ = do
    opts <- getSandboxOpts <$> getSandbox
    void $ system $ "ghci" ++ " " ++ opts ++ " " ++ intercalate " " args
