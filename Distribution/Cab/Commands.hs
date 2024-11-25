module Distribution.Cab.Commands (
    FunctionCommand,
    Option (..),
    deps,
    revdeps,
    installed,
    outdated,
    uninstall,
    search,
    genpaths,
    check,
    initSandbox,
    add,
    ghci,
) where

import qualified Control.Exception as E
import Control.Monad (forM_, unless, void, when)
import Data.Char (toLower)
import Data.List (intercalate, isPrefixOf)
import qualified Data.Map as M
import Distribution.Cab.GenPaths
import Distribution.Cab.PkgDB
import Distribution.Cab.Printer
import Distribution.Cab.Sandbox
import Distribution.Cab.VerDB
import Distribution.Cab.Version
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess, system)

----------------------------------------------------------------

type FunctionCommand = [String] -> [Option] -> [String] -> IO ()

data Option
    = OptNoharm
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
    | OptImport String
    | OptStatic
    | OptFuture
    | OptDebug
    | OptAllowNewer
    | OptCleanUp
    deriving (Eq, Show)

----------------------------------------------------------------

search :: FunctionCommand
search [x] _ _ = do
    nvls <- toList <$> getVerDB AllRegistered
    forM_ (lok nvls) $ \(n, v) -> putStrLn $ n ++ " " ++ verToString v
  where
    key = map toLower x
    sat (n, _) = key `isPrefixOf` map toLower n
    lok = filter sat
search _ _ _ = do
    hPutStrLn stderr "One search-key should be specified."
    exitFailure

----------------------------------------------------------------

installed :: FunctionCommand
installed _ opts _ = do
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
outdated _ opts _ = do
    pkgs <- toPkgInfos <$> getDB opts
    verDB <- toMap <$> getVerDB InstalledOnly
    let del = OptCleanUp `elem` opts
    forM_ pkgs $ \p -> case M.lookup (nameOfPkgInfo p) verDB of
        Nothing -> return ()
        Just ver -> do
            let comp = verOfPkgInfo p `compare` ver
            when (dated comp) $ do
                if del
                    then do
                        let (nm, vr) = pairNameOfPkgInfo p
                        uninstall [nm, vr] [OptRecursive] [] `E.catch` \(E.SomeException _) -> return ()
                    else
                        putStrLn $ fullNameOfPkgInfo p ++ showIneq comp ++ verToString ver
  where
    dated LT = True
    dated GT = OptFuture `elem` opts
    dated EQ = False
    showIneq LT = " < "
    showIneq GT = " > "
    showIneq EQ = error "Packages have equal versions"

getDB :: [Option] -> IO PkgDB
getDB opts
    | optall = getSandbox >>= getPkgDB
    | otherwise = getSandbox >>= getUserPkgDB
  where
    optall = OptAll `elem` opts

----------------------------------------------------------------

uninstall :: FunctionCommand
uninstall nmver opts _ = do
    userDB <- getSandbox >>= getUserPkgDB
    pkg <- lookupPkg nmver userDB
    let sortedPkgs = topSortedPkgs pkg userDB
    if onlyOne && length sortedPkgs /= 1
        then do
            hPutStrLn stderr "The following packages depend on this. Use the \"-r\" option."
            mapM_ (hPutStrLn stderr . fullNameOfPkgInfo) (init sortedPkgs)
        else do
            unless doit $
                putStrLn "The following packages are deleted without the \"-n\" option."
            mapM_ (purge doit opts) sortedPkgs
  where
    onlyOne = OptRecursive `notElem` opts
    doit = OptNoharm `notElem` opts

purge :: Bool -> [Option] -> PkgInfo -> IO ()
purge doit opts pkgInfo = do
    sandboxOpts <- (makeOptList . getSandboxOpts2) <$> getSandbox
    dirs <- getDirs nameVer sandboxOpts
    unregister doit opts nameVer
    mapM_ unregisterInternal $ findInternalLibs pkgInfo
    mapM_ (removeDir doit) dirs
  where
    unregisterInternal subname = unregister doit opts (nm, ver)
      where
        nm = "z-" ++ name ++ "-z-" ++ subname
    nameVer@(name, ver) = pairNameOfPkgInfo pkgInfo
    makeOptList "" = []
    makeOptList x = [x]

getDirs :: (String, String) -> [String] -> IO [FilePath]
getDirs (name, ver) sandboxOpts = do
    importDirs <- queryGhcPkg "import-dirs"
    haddock <- map docDir <$> queryGhcPkg "haddock-html"
    return $ topDir $ importDirs ++ haddock
  where
    nameVer = name ++ "-" ++ ver
    queryGhcPkg field = do
        let options = ["field"] ++ sandboxOpts ++ [nameVer, field]
        ws <- words <$> readProcess "ghc-pkg" options ""
        return $ case ws of
            [] -> []
            (_ : xs) -> xs
    docDir dir
        | takeFileName dir == "html" = takeDirectory dir
        | otherwise = dir
    topDir [] = []
    topDir ds@(dir : _)
        | takeFileName top == nameVer = top : ds
        | otherwise = ds
      where
        top = takeDirectory dir

removeDir :: Bool -> FilePath -> IO ()
removeDir doit dir = do
    exist <- doesDirectoryExist dir
    when exist $ do
        putStrLn $ "Deleting " ++ dir
        when doit $ removeDirectoryRecursive dir

unregister :: Bool -> [Option] -> (String, String) -> IO ()
unregister doit _ (name, ver) =
    if doit
        then do
            putStrLn $ "Deleting " ++ name ++ " " ++ ver
            sandboxOpts <- getSandboxOpts2 <$> getSandbox
            when doit $ void . system $ script sandboxOpts
        else
            putStrLn $ name ++ " " ++ ver
  where
    script sandboxOpts = "ghc-pkg unregister " ++ sandboxOpts ++ " " ++ name ++ "-" ++ ver

----------------------------------------------------------------

genpaths :: FunctionCommand
genpaths _ _ _ = genPaths

----------------------------------------------------------------

check :: FunctionCommand
check _ _ _ = do
    sandboxOpts <- getSandboxOpts2 <$> getSandbox
    void . system $ script sandboxOpts
  where
    script sandboxOpts = "ghc-pkg check -v " ++ sandboxOpts

----------------------------------------------------------------

deps :: FunctionCommand
deps nmver opts _ = printDepends nmver opts printDeps

revdeps :: FunctionCommand
revdeps nmver opts _ = printDepends nmver opts printRevDeps

printDepends
    :: [String]
    -> [Option]
    -> (Bool -> Bool -> PkgDB -> Int -> PkgInfo -> IO ())
    -> IO ()
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
lookupPkg [name, ver] db = checkOne $ lookupByVersion name ver db
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
initSandbox [] _ _ = void . system $ "cabal v1-sandbox init"
initSandbox [path] _ _ = void . system $ "cabal v1-sandbox init --sandbox " ++ path
initSandbox _ _ _ = do
    hPutStrLn stderr "Only one argument is allowed"
    exitFailure

----------------------------------------------------------------

add :: FunctionCommand
add [src] _ _ = void . system $ "cabal v1-sandbox add-source " ++ src
add _ _ _ = do
    hPutStrLn stderr "A source path be specified."
    exitFailure

----------------------------------------------------------------

ghci :: FunctionCommand
ghci args _ options = do
    sbxOpts <- getSandboxOpts <$> getSandbox
    void $
        system $
            "ghci" ++ " " ++ sbxOpts ++ " " ++ intercalate " " (options ++ args)
