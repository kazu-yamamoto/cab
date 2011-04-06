module Main where

import CmdDB
import Control.Exception
import Control.Monad
import Data.Maybe
import System.Cmd
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import Types
import Utils

----------------------------------------------------------------

main :: IO ()
main = flip catches handlers $ do
    oargs <- getArgs
    let pargs = parseArgs getOptDB oargs
    checkOptions1 pargs illegalOptionsAndExit
    let Right (args,opts) = pargs
    when (args == []) helpAndExit
    let act:params = args
        mcmdspec = commandSpecByName act commandDB
    when (isNothing mcmdspec) (illegalCommandAndExit act)
    let Just cmdspec = mcmdspec
    checkOptions2 opts cmdspec oargs illegalOptionsAndExit
    run cmdspec params opts
  where
    handlers = [Handler handleExit]
    handleExit :: ExitCode -> IO ()
    handleExit _ = return ()

----------------------------------------------------------------

parseArgs :: [GetOptSpec] -> [Arg] -> ParsedArgs
parseArgs db args = case getOpt' Permute db args of
    (o,n,[],[])      -> Right (n,o)
    (_,_,unknowns,_) -> Left unknowns

checkOptions1 :: ParsedArgs -> ([UnknownOpt] -> IO ()) -> IO ()
checkOptions1 (Left es) func = func es
checkOptions1 _ _            = return ()

checkOptions2 :: [Option] -> CommandSpec -> [Arg] -> ([UnknownOpt] -> IO ()) -> IO ()
checkOptions2 opts cmdspec oargs func = do
    let unknowns = check specified supported
    when (unknowns /= []) $ func (concatMap (resolveOptionString oargs) unknowns)
  where
    check [] _     = []
    check (x:xs) ys
      | x `elem` ys = check xs ys
      | otherwise   = x : check xs ys
    specified = map toSwitch opts
    supported = map fst $ switches cmdspec

----------------------------------------------------------------

run :: CommandSpec -> [Arg] -> [Option] -> IO ()
run cmdspec params opts = case routing cmdspec of
    RouteFunc func -> func cmdspec params opts
    RouteProc subcmd subargs -> callProcess subcmd subargs params opts (switches cmdspec)

callProcess :: String -> [String] -> [Arg] -> [Option] -> [SwitchSpec] -> IO ()
callProcess pro args0 args1 opts sws = system script >> return ()
  where
    swchs = optionsToString opts sws
    pro'
      | SwSandbox `elem` (map toSwitch opts) = "cabal-dev"
      | otherwise                            = pro
    script = joinBy " " $ pro' : swchs ++ args0 ++ cat args1
    cat [pkg,ver] = [pkg ++ "-" ++ ver]
    cat x         = x
