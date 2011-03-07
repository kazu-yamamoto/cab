module Help where

import Commands
import Data.List
import Program
import Data.Maybe

displayHelp :: [String] -> IO ()
displayHelp [] = displayHelpAll
displayHelp (x:_) = case getCommand x commandDB of
    Help -> displayHelpAll
    com  -> displayHelpCommand com

displayHelpAll :: IO ()
displayHelpAll = do
    putStrLn $ programName ++ " " ++ " -- " ++ description
    putStrLn ""
    putStrLn $ "Version: " ++ version
    putStrLn "Usage:"
    putStrLn $ "\t" ++ programName ++ " help"
    putStrLn $ "\t" ++ programName ++ " <command> [args...]"
    putStrLn $ "\t  where"
    printCommands (getCommands commandDB)
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

displayHelpCommand :: Command -> IO ()
displayHelpCommand com = putStrLn $ document spec
  where
    spec = fromJust $ commandSpecByCommand com commandDB

helpCommandNumber :: Int
helpCommandNumber = 10

split :: Int -> [a] -> [[a]]
split _ [] = []
split n ss = x : split n rest
  where
    (x,rest) = splitAt n ss
