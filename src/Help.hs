module Help (
    helpAndExit
  , helpCommandAndExit
  ) where

import Control.Monad (forM_)
import Data.List (intersperse)
import Distribution.Cab
import System.Exit (exitSuccess)

import Commands
import Doc
import Options
import Program
import Types

----------------------------------------------------------------

helpCommandAndExit :: FunctionCommand
helpCommandAndExit [] _ _ = helpAndExit
helpCommandAndExit (cmd:_) _ _ = do
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
    mcmdspec = commandSpecByName cmd (commandDB helpCommandAndExit)

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
    printCommands . getCommands . commandDB $ helpCommandAndExit
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

-- |
-- >>> split 4 "0123457689"
-- ["0123","4576","89"]
split :: Int -> [a] -> [[a]]
split _ [] = []
split n ss = x : split n rest
  where
    (x,rest) = splitAt n ss
