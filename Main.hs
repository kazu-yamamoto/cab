module Main where

import Commands
import Control.Exception
import Data.List
import Data.Maybe
import Help
import Options
import System.Cmd
import System.Console.GetOpt
import System.Environment (getArgs)

parseArgs :: [OptDescr (Options -> Options)] -> [String] -> (Options, [String])
parseArgs spec argv
    = case getOpt Permute spec argv of
        (o,n,[]  ) -> (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> error "XXX" -- FIXME

analyzeArgs :: Options -> [String] -> (Command, [String], Options)
analyzeArgs opt []     = (Help, [], opt)
analyzeArgs opt xs
  | help opt           = (Help, xs, opt)
analyzeArgs opt (x:xs) = (getCommand x commandDB, xs, opt)

main :: IO ()
main = flip catches handlers $ do
    args <- getArgs
    let (cmd, params, opts) = uncurry analyzeArgs $ parseArgs argSpec args
    if cmd == Help
        then displayHelp params
        else do
            let spec = fromJust $ commandSpecByCommand cmd commandDB
                route = routing spec
            case route of
                RouteFunc func -> func params
                RouteProc subcmd subargs -> callProcess subcmd subargs params opts
  where
    handlers = undefined

callProcess :: String -> [String] -> [String] -> Options -> IO ()
callProcess cmd args0 args1 _ = system script >> return ()
  where
    script = concat . intersperse " " $ cmd : args0 ++ args1
