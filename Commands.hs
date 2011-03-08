module Commands where

import Data.List
import Outdated
import Program
import System.Exit
import Types
import System.IO

commandDB :: CommandDB
commandDB = [
    CommandSpec {
         command = Sync
       , commandNames = ["sync", "update"]
       , document = "Fetchinwg the latest package index"
       , routing = RouteProc "cabal" ["update"]
       , options = []
       }
  , CommandSpec {
         command = Install
       , commandNames = ["install"]
       , document = "Install packages"
       , routing = RouteProc "cabal" ["install"]
       , options = [
           OptionSpec {
                option = OptNoHarm
              , optionNames = ["-n", "--dry-run"]
              , actualOption = "--dry-run"
              }
         ]
       }
  , CommandSpec {
         command = Uninstall
       , commandNames = ["uninstall"]
       , document = "Uninstalling packages"
       , routing = RouteProc "ghc-pkg" ["unregister"]
       , options = [
           OptionSpec {
                option = OptRecursive
              , optionNames = ["-r", "--follow-dependents"]
              , actualOption = []
              }
         ]
       }
  , CommandSpec {
         command = Installed
       , commandNames = ["installed", "list"]
       , document = "Listing installed packages"
       , routing = RouteProc "ghc-pkg" ["list"]
       , options = []
       }
  , CommandSpec {
         command = Configure
       , commandNames = ["configure", "conf"]
       , document = "Configuring a cabal package"
       , routing = RouteProc "cabal" ["configure"]
       , options = []
       }
  , CommandSpec {
         command = Build
       , commandNames = ["build"]
       , document = "Building a cabal package"
       , routing = RouteProc "cabal" ["build"]
       , options = []
       }
  , CommandSpec {
         command = Clean
       , commandNames = ["clean"]
       , document = "Cleaning a build directory"
       , routing = RouteProc "cabal" ["clean"]
       , options = []
       }
  , CommandSpec {
         command = Outdated
       , commandNames = ["outdated"]
       , document = "Displaying outdated packages"
       , routing = RouteFunc outdated
       , options = []
       }
  , CommandSpec {
         command = Info
       , commandNames = ["info"]
       , document = "Display information of a package"
       , routing = RouteProc "cabal" ["info"]
       , options = []
       }
  , CommandSpec {
         command = Help
       , commandNames = ["help"]
       , document = undefined
       , routing = RouteFunc helpCommandAndExit
       , options = []
       }
  ]

----------------------------------------------------------------

commandSpecByName :: String -> CommandDB -> Maybe CommandSpec
commandSpecByName _ [] = Nothing
commandSpecByName x (ent:ents)
    | x `elem` commandNames ent = Just ent
    | otherwise                 = commandSpecByName x ents

optionSpecByName :: String -> OptionDB -> Maybe OptionSpec
optionSpecByName _ [] = Nothing
optionSpecByName x (ent:ents)
    | x `elem` optionNames ent = Just ent
    | otherwise                = optionSpecByName x ents

----------------------------------------------------------------

-- FIXME: more description of a command
helpCommandAndExit :: FunctionCommand
helpCommandAndExit _ [] _ = helpAndExit
helpCommandAndExit _ (cmd:_) _ = do
    case mcmdspec of
        Nothing -> helpAndExit
        Just cmdspec -> putStrLn $ document cmdspec
    exitSuccess
  where
    mcmdspec = commandSpecByName cmd commandDB

helpAndExit :: IO ()
helpAndExit = do
    putStrLn $ programName ++ " " ++ " -- " ++ description
    putStrLn ""
    putStrLn $ "Version: " ++ version
    putStrLn "Usage:"
    putStrLn $ "\t" ++ programName ++ " help"
    putStrLn $ "\t" ++ programName ++ " <command> [args...]"
    putStrLn $ "\t  where"
    printCommands (getCommands commandDB)
    exitSuccess
  where
    getCommands = map concat
                . split helpCommandNumber
                . intersperse ","
                . map head
                . map commandNames
    printCommands [] = return ()
    printCommands (x:xs) = do
        putStrLn $ "\t    <command> = " ++ x
        mapM_ (\cmds -> putStrLn $ "\t                " ++ cmds) xs

helpCommandNumber :: Int
helpCommandNumber = 10

split :: Int -> [a] -> [[a]]
split _ [] = []
split n ss = x : split n rest
  where
    (x,rest) = splitAt n ss

illegalCommandAndExit :: String -> IO ()
illegalCommandAndExit x = do
    hPutStrLn stderr $ "Illegal command: " ++ x
    exitFailure

illegalOptionsAndExit :: [String] -> IO ()
illegalOptionsAndExit xs = do -- FixME
    hPutStrLn stderr $ "Illegal options: " ++ concat (intersperse " " xs)
    exitFailure
