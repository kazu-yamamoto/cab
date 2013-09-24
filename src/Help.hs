module Help (
    helpAndExit
  , helpCommandAndExit
  , illegalOptionsAndExit
  , illegalCommandAndExit
  , usageDocAlias
  , optionDoc
  ) where

import Control.Monad
import Data.List
import Distribution.Cab
import System.Console.GetOpt
import System.Exit
import System.IO

import Commands
import Options
import Program
import Types

----------------------------------------------------------------

helpCommandAndExit :: FunctionCommand
helpCommandAndExit [] _ = helpAndExit
helpCommandAndExit (cmd:_) _ = do
    case mcmdspec of
        Nothing -> helpAndExit
        Just cmdspec -> do
            let (usage,doc,alias) = usageDocAlias cmdspec
            putStrLn $ "Usage: " ++ usage
            putStr "\n"
            putStrLn $ doc
            putStr "\n"
            putStrLn $ "Aliases: " ++ alias
            putStr "\n"
            printOptions cmdspec
    exitSuccess
  where
    mcmdspec = commandSpecByName cmd commandDB

usageDocAlias :: CommandSpec -> (String, String, String)
usageDocAlias cmdspec = (usage,doc,alias)
  where
    usage = cmd ++ " " ++ showOptions ++ showArgs
    doc = document cmdspec
    alias = showAliases cmdspec
    cmd:_ = commandNames cmdspec
    options = opts cmdspec
    showOptions
      | null options = ""
      | otherwise    = "[" ++ intercalate "] [" (concatMap (masterOption optionDB) (opts cmdspec)) ++ "]"
    showArgs = maybe "" (" " ++) $ manual cmdspec
    opts = map fst . switches
    masterOption [] _ = []
    masterOption (spec:specs) o
      | fst spec == o = optionName spec : masterOption specs o
      | otherwise     = masterOption specs o
    showAliases = intercalate ", " . tail . commandNames

printOptions :: CommandSpec -> IO ()
printOptions cmdspec =
    forM_ opts (printOption optionDB)
  where
    opts = map fst $ switches cmdspec
    printOption [] _ = return ()
    printOption (spec:specs) o
      | fst spec == o = do
          let (key,doc) = optionDoc spec
          putStrLn $ key ++ "\t" ++ doc
      | otherwise        = printOption specs o

optionDoc :: OptionSpec -> (String, String)
optionDoc spec = (key,doc)
  where
    key = intercalate ", " . reverse . optionNames $ spec
    doc = optionDesc spec

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
    hPutStrLn stderr $ "Illegal options: " ++ intercalate " " xs
    exitFailure

----------------------------------------------------------------

-- |
-- >>> split 4 "0123457689"
-- ["0123","4576","89"]
split :: Int -> [a] -> [[a]]
split _ [] = []
split n ss = x : split n rest
  where
    (x,rest) = splitAt n ss
