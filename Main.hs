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
    let Right flags = eoptspecs
    run cmdspec params flags
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

parseOptions :: CommandSpec -> [String] -> Either [String] Flags
parseOptions cmdspc opts = check opts [] defaultFlags
  where
    optMvals = options cmdspc
    check [] [] fg = Right fg
    check [] es _  = Left es
    check (o:os) es fg = case optionSpecByName o optionDB of
        Nothing -> check os (o:es) fg
        Just x  -> case lookup (option x) optMvals of
            Nothing   -> check os (o:es) fg
            Just mval -> check os es (updateFlags (option x) mval fg)

checkHelp :: [String] -> [String] -> FunctionCommand -> IO ()
checkHelp args opts func
  | "-h"     `elem` opts
 || "--help" `elem` opts = func undefined args undefined
  | otherwise            = return ()

checkOptions :: Either [String] Flags -> ([String] -> IO ()) -> IO ()
checkOptions (Left xs) func = func xs
checkOptions _ _            = return ()

----------------------------------------------------------------

run :: CommandSpec -> [String] -> Flags -> IO ()
run cmdspec params flags = case routing cmdspec of
    RouteFunc func -> func cmdspec params flags
    RouteProc subcmd subargs -> callProcess subcmd subargs params flags

callProcess :: String ->[String] -> [String] -> Flags -> IO ()
callProcess pro args0 args1 flags = system script >> return ()
  where
    opts = flagsToOptions flags
    script = concat . intersperse " " $ pro : opts ++ args0 ++ args1
