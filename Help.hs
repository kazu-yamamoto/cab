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
    putStrLn ""
    putStrLn $ "\t <command> = " ++ extractCommands commandDB
  where
    extractCommands = concat . intersperse "," . map head . map commandNames
    -- FIXME take 5 or so

displayHelpCommand :: Command -> IO ()
displayHelpCommand com = putStrLn $ document spec
  where
    spec = fromJust $ commandSpecByCommand com commandDB
