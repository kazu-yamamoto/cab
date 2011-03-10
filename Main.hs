module Main where

import Control.Applicative
import CmdDB
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import System.Cmd
import System.Environment (getArgs)
import Types
import System.Exit

----------------------------------------------------------------

main :: IO ()
main = flip catches handlers $ do
    (args,opts) <- argsOpts <$> getArgs
    when (args == []) helpAndExit
    checkHelp args opts helpCommandAndExit
    let act = head args
        mcmdspec = commandSpecByName act commandDB
    when (isNothing mcmdspec) (illegalCommandAndExit act)
    let Just cmdspec = mcmdspec
        params = tail args
        eoptspecs = parseOptions cmdspec opts
    checkOptions eoptspecs illegalOptionsAndExit
    let Right optspecs = eoptspecs
    run cmdspec params optspecs
  where
    handlers = [Handler handleExit]
    handleExit :: ExitCode -> IO ()
    handleExit _ = return ()

----------------------------------------------------------------

argsOpts :: [String] -> ([String],[String])
argsOpts args = (args', opts)
  where
    args' = filter (not . isPrefixOf "-") args
    opts = filter (isPrefixOf "-") args

parseOptions :: CommandSpec -> [String] -> Either [String] [OptionSpec]
parseOptions cmdspc opts = check opts [] []
  where
    optspec = options cmdspc
    check [] [] ys = Right ys
    check [] xs _  = Left xs
    check (o:os) xs ys = case optionSpecByName o optspec of
        Nothing -> check os (o:xs) ys
        Just s  -> check os xs (s:ys)

checkHelp :: [String] -> [String] -> FunctionCommand -> IO ()
checkHelp args opts func
  | "-h"     `elem` opts
 || "--help" `elem` opts = func undefined args undefined
  | otherwise            = return ()

checkOptions :: Either [String] [OptionSpec] -> ([String] -> IO ()) -> IO ()
checkOptions (Left xs) func = func xs
checkOptions _ _            = return ()

----------------------------------------------------------------

run :: CommandSpec -> [String] -> [OptionSpec] -> IO ()
run cmdspec params opts = case routing cmdspec of
    RouteFunc func -> func cmdspec params opts
    RouteProc subcmd subargs -> callProcess subcmd subargs params opts

callProcess :: String ->[String] -> [String] -> [OptionSpec] -> IO ()
callProcess pro args0 args1 opts = system script >> return ()
  where
    script = concat . intersperse " " $ pro : args0 ++ args1
