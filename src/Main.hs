module Main where

import Control.Exception (Handler(..))
import qualified Control.Exception as E
import Control.Monad
import Data.Maybe
import Distribution.Cab.Types
import Distribution.Cab.Utils
import System.Cmd
import System.Console.GetOpt
import System.Environment
import System.Exit

import CmdDB
import Env
import Types

----------------------------------------------------------------

main :: IO ()
main = flip E.catches handlers $ do
    unsetEnv "GHC_PACKAGE_PATH"
    oargs <- getArgs
    let pargs = parseArgs getOptDB oargs
    checkOptions1 pargs illegalOptionsAndExit
    let Right (args,opts0) = pargs
    when (args == []) helpAndExit
    when (OptHelp `elem` opts0) $ helpCommandAndExit args undefined
    let opts1 = filter (/= OptHelp) opts0
        act:params = args
        mcmdspec = commandSpecByName act commandDB
    when (isNothing mcmdspec) (illegalCommandAndExit act)
    let Just cmdspec = mcmdspec
    checkOptions2 opts1 cmdspec oargs illegalOptionsAndExit
    run cmdspec params opts1
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
    RouteFunc func     -> func params opts
    RouteCabal subargs -> callProcess pro subargs params opts sws
  where
    pro = "cabal"
    sws = switches cmdspec

callProcess :: String -> [String] -> [Arg] -> [Option] -> [SwitchSpec] -> IO ()
callProcess pro args0 args1 opts sws = void . system $ script
  where
    swchs = optionsToString opts sws
    script = joinBy " " $ pro : args0 ++ cat args1 ++ swchs
    cat [pkg,ver] = [pkg ++ "-" ++ ver]
    cat x         = x
