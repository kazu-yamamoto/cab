module CmdDB where

import Control.Monad
import Data.List
import Distribution.Cab
import System.Console.GetOpt
import System.Exit
import System.IO

import Program
import Types

----------------------------------------------------------------

commandDB :: CommandDB
commandDB = [
    CommandSpec {
         command = Sync
       , commandNames = ["sync", "update"]
       , document = "Fetch the latest package index"
       , routing = RouteCabal ["update"]
       , switches = []
       , manual = Nothing
       }
  , CommandSpec {
         command = Install
       , commandNames = ["install"]
       , document = "Install packages"
       , routing = RouteCabal ["install"]
       , switches = [(SwNoharm, Just "--dry-run -v")
                     -- FIXME cabal-dev not support --dry-run
                    ,(SwFlag, Just "--flags")
                    ]
       , manual = Just "[<package> [<ver>]]"
       }
  , CommandSpec {
         command = Uninstall
       , commandNames = ["uninstall", "delete", "remove", "unregister"]
       , document = "Uninstall packages"
       , routing = RouteFunc uninstall
       , switches = [(SwNoharm, Nothing)
                    ,(SwRecursive, Nothing)
                    ] -- don't allow SwAll
       , manual = Just "<package> [<ver>]"
       }
  , CommandSpec {
         command = Installed
       , commandNames = ["installed", "list"]
       , document = "List installed packages"
       , routing = RouteFunc installed
       , switches = [(SwAll, Nothing)
                    ,(SwRecursive, Nothing)
                    ,(SwInfo, Nothing)
                    ]
       , manual = Nothing
       }
  , CommandSpec {
         command = Configure
       , commandNames = ["configure", "conf"]
       , document = "Configure a cabal package"
       , routing = RouteCabal ["configure"]
       , switches = [(SwFlag, Just "--flags")
                    ,(SwTest, Just "--enable-tests")
                    ,(SwBench, Just "--enable-benchmarks")
                    ]
       , manual = Nothing
       }
  , CommandSpec {
         command = Build
       , commandNames = ["build"]
       , document = "Build a cabal package"
       , routing = RouteCabal ["build"]
       , switches = []
       , manual = Nothing
       }
  , CommandSpec {
         command = Clean
       , commandNames = ["clean"]
       , document = "Clean up a build directory"
       , routing = RouteCabal ["clean"]
       , switches = []
       , manual = Nothing
       }
  , CommandSpec {
         command = Outdated
       , commandNames = ["outdated"]
       , document = "Display outdated packages"
       , routing = RouteFunc outdated
       , switches = [(SwAll, Nothing)]
       , manual = Nothing
       }
  , CommandSpec {
         command = Info
       , commandNames = ["info"]
       , document = "Display information of a package"
       , routing = RouteCabal ["info"]
       , switches = []
       , manual = Just "<package> [<ver>]"
       }
  , CommandSpec {
         command = Sdist
       , commandNames = ["sdist", "pack"]
       , document = "Make tar.gz for source distribution"
       , routing = RouteCabal ["sdist"]
       , switches = []
       , manual = Nothing
       }
  , CommandSpec {
         command = Upload
       , commandNames = ["upload", "up"]
       , document = "Uploading tar.gz to HackageDB"
       , routing = RouteCabal ["upload"]
       , switches = [(SwNoharm, Just "-c")]
       , manual = Nothing
       }
  , CommandSpec {
         command = Unpack
       , commandNames = ["unpack"]
       , document = "Untar a package in the current directory"
       , routing = RouteCabal ["unpack"]
       , switches = []
       , manual = Just "<package> [<ver>]"
       }
  , CommandSpec {
         command = Deps
       , commandNames = ["deps"]
       , document = "Show dependencies of this package"
       , routing = RouteFunc deps
       , switches = [(SwRecursive, Nothing)
                    ,(SwAll, Nothing)
                    ,(SwInfo, Nothing)
                    ]
       , manual = Just "<package> [<ver>]"
       }
  , CommandSpec {
         command = RevDeps
       , commandNames = ["revdeps", "dependents"]
       , document = "Show reverse dependencies of this package"
       , routing = RouteFunc revdeps
       , switches = [(SwRecursive, Nothing)
                    ,(SwAll, Nothing)
                    ,(SwInfo, Nothing)
                    ]
       , manual = Just "<package> [<ver>]"
       }
  , CommandSpec {
         command = Check
       , commandNames = ["check"]
       , document = "Check consistency of packages"
       , routing = RouteFunc check
       , switches = []
       , manual = Nothing
       }
  , CommandSpec {
         command = GenPaths
       , commandNames = ["genpaths", "genpath"]
       , document = "Generate Paths_<pkg>.hs"
       , routing = RouteFunc genpaths
       , switches = []
       , manual = Nothing
       }
  , CommandSpec {
         command = Search
       , commandNames = ["search"]
       , document = "Search available packages by package name"
       , routing = RouteFunc search
       , switches = []
       , manual = Just "<key>"
       }
  , CommandSpec {
         command = Add
       , commandNames = ["add", "add-source"]
       , document = "Add a source directory"
       , routing = RouteFunc add
       , switches = []
       , manual = Just "<source>"
       }
  , CommandSpec {
         command = Test
       , commandNames = ["test"]
       , document = "Run tests"
       , routing = RouteCabal ["test"]
       , switches = []
       , manual = Nothing
       }
  , CommandSpec {
         command = Bench
       , commandNames = ["bench"]
       , document = "Run benchmarks"
       , routing = RouteCabal ["bench"]
       , switches = []
       , manual = Nothing
       }
  , CommandSpec {
         command = Doc
       , commandNames = ["doc", "haddock", "man"]
       , document = "Generate manuals"
       , routing = RouteCabal ["haddock", "--hyperlink-source"]
       , switches = []
       , manual = Nothing
       }
  , CommandSpec {
         command = Ghci
       , commandNames = ["ghci"]
       , document = "Run GHCi within a sandbox"
       , routing = RouteFunc ghci
       , switches = []
       , manual = Nothing
       }
  , CommandSpec {
         command = Help
       , commandNames = ["help"]
       , document = "Display the help message of the command"
       , routing = RouteFunc helpCommandAndExit
       , switches = []
       , manual = Just "[<command>]"
       }
  ]

----------------------------------------------------------------

commandSpecByName :: String -> CommandDB -> Maybe CommandSpec
commandSpecByName _ [] = Nothing
commandSpecByName x (ent:ents)
    | x `elem` commandNames ent = Just ent
    | otherwise                 = commandSpecByName x ents

----------------------------------------------------------------

getOptDB :: GetOptDB
getOptDB = [
    Option ['n'] ["dry-run"]
      (NoArg OptNoharm)
      "Run without destructive operations"
  , Option ['r'] ["recursive"]
      (NoArg OptRecursive)
      "Follow dependencies recursively"
  , Option ['a'] ["all"]
      (NoArg OptAll)
      "Show global packages in addition to user packages"
  , Option ['i'] ["info"]
      (NoArg OptInfo)
      "Show license and author information"
  , Option ['f'] ["flags"]
      (ReqArg OptFlag "<flags>")
      "Specify flags"
  , Option ['t'] ["test"]
      (NoArg OptTest)
      "Enable test"
  , Option ['b'] ["bench"]
      (NoArg OptBench)
      "Enable benchmark"
  , Option ['h'] ["help"]
      (NoArg OptHelp)
      "Show help message"
  ]

optionDB :: OptionDB
optionDB = zip [SwNoharm,SwRecursive,SwAll,SwInfo,SwFlag,SwTest,SwBench] getOptDB

----------------------------------------------------------------

optionName :: OptionSpec -> String
optionName (_,Option (c:_) _ (ReqArg _ arg) _) = '-':c:' ':arg
optionName (_,Option (c:_) _ _ _)              = '-':[c]
optionName _                                   = ""

optionNames :: OptionSpec -> [String]
optionNames (_,Option (c:_) (s:_) _ _) = ['-':[c],'-':'-':s]
optionNames _                          = []

optionDesc :: OptionSpec -> String
optionDesc (_,Option _ _ _ desc) = desc

getOptNames :: GetOptSpec -> (String,String)
getOptNames (Option (c:_) (s:_) _ _) = ('-':[c],'-':'-':s)
getOptNames _                        = error "getOptNames"

resolveOptionString :: [Arg] -> Switch -> [UnknownOpt]
resolveOptionString oargs sw = case lookup sw optionDB of
    Nothing    -> error "resolveOptionString"
    Just gspec -> let (s,l) = getOptNames gspec
                  in checkShort s ++ checkLong l
  where
    checkShort s = filter (==s) oargs
    checkLong  l = filter (l `isPrefixOf`) oargs

optionsToString :: [Option] -> SwitchDB -> [String]
optionsToString opts swdb = concatMap suboption opts
  where
    suboption opt = case lookup (toSwitch opt) swdb of
        Nothing       -> []
        Just Nothing  -> []
        Just (Just x) -> case opt of
            OptFlag flags  -> [x++"="++flags]
            _              -> [x]

----------------------------------------------------------------

helpCommandAndExit :: FunctionCommand
helpCommandAndExit [] _ = helpAndExit
helpCommandAndExit (cmd:_) _ = do
    case mcmdspec of
        Nothing -> helpAndExit
        Just cmdspec -> do
            putStrLn $ "Usage: " ++ cmd ++ " " ++ showOptions cmdspec ++ showArgs cmdspec
            putStr "\n"
            putStrLn $ document cmdspec
            putStr "\n"
            putStrLn $ "Aliases: " ++ showAliases cmdspec
            putStr "\n"
            printOptions cmdspec
    exitSuccess
  where
    mcmdspec = commandSpecByName cmd commandDB
    showOptions cmdspec = "[" ++ joinBy "] [" (concatMap (masterOption optionDB) (opts cmdspec)) ++ "]"
    showArgs cmdspec = maybe "" (" " ++) $ manual cmdspec
    opts = map fst . switches
    masterOption [] _ = []
    masterOption (spec:specs) o
      | fst spec == o = optionName spec : masterOption specs o
      | otherwise     = masterOption specs o
    showAliases = joinBy ", " . commandNames

printOptions :: CommandSpec -> IO ()
printOptions cmdspec =
    forM_ opts (printOption optionDB)
  where
    opts = map fst $ switches cmdspec
    printOption [] _ = return ()
    printOption (spec:specs) o
      | fst spec == o =
          putStrLn $ (joinBy ", " . reverse . optionNames $ spec)
                   ++ "\t" ++ optionDesc spec
      | otherwise        = printOption specs o

----------------------------------------------------------------

helpAndExit :: IO ()
helpAndExit = do
    putStrLn $ programName ++ " " ++ " -- " ++ description
    putStrLn ""
    putStrLn $ "Version: " ++ showVersion version
    putStrLn "Usage:"
    putStrLn $ "\t" ++ programName
    putStrLn $ "\t" ++ programName ++ " <command> [args...]"
    putStrLn   "\t  where"
    printCommands (getCommands commandDB)
    exitSuccess
  where
    getCommands = map concat
                . split helpCommandNumber
                . intersperse ", "
                . map (head . commandNames)
    printCommands [] = return ()
    printCommands (x:xs) = do
        putStrLn $ "\t    <command> = " ++ x
        mapM_ (\cmds -> putStrLn $ "\t                " ++ cmds) xs

helpCommandNumber :: Int
helpCommandNumber = 10

----------------------------------------------------------------

illegalCommandAndExit :: String -> IO ()
illegalCommandAndExit x = do
    hPutStrLn stderr $ "Illegal command: " ++ x
    exitFailure

----------------------------------------------------------------

illegalOptionsAndExit :: [UnknownOpt] -> IO ()
illegalOptionsAndExit xs = do -- FixME
    hPutStrLn stderr $ "Illegal options: " ++ joinBy " " xs
    exitFailure
