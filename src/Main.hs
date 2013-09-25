module Main where

import Control.Exception (Handler(..))
import qualified Control.Exception as E
import Control.Monad
import Data.Maybe
import Data.List (isPrefixOf, intercalate)
import Distribution.Cab
import System.Cmd
import System.Console.GetOpt
import System.Environment
import System.Exit

import Commands
import Help
import Options
import Types

----------------------------------------------------------------

main :: IO ()
main = flip E.catches handlers $ do
    oargs <- getArgs
    let pargs = parseArgs getOptDB oargs
    checkOptions1 pargs illegalOptionsAndExit
    let Right (args,opts0) = pargs
    when (args == []) helpAndExit
    when (OptHelp `elem` opts0) $ helpCommandAndExit args [] []
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
    let unknowns = chk specified supported
    when (unknowns /= []) $ func (concatMap (resolveOptionString oargs) unknowns)
  where
    chk [] _     = []
    chk (x:xs) ys
      | x `elem` ys = chk xs ys
      | otherwise   = x : chk xs ys
    specified = map toSwitch opts
    supported = map fst $ switches cmdspec

----------------------------------------------------------------

run :: CommandSpec -> [Arg] -> [Option] -> IO ()
run cmdspec params opts = case routing cmdspec of
    RouteFunc func     -> func params opts options
    RouteCabal subargs -> callProcess pro subargs params options
  where
    pro = "cabal"
    sws = switches cmdspec
    options = optionsToString opts sws

callProcess :: String -> [String] -> [Arg] -> [String] -> IO ()
callProcess pro args0 args1 options = void . system $ script
  where
    script = intercalate " " $ pro : args0 ++ cat args1 ++ options
    cat [pkg,ver] = [pkg ++ "-" ++ ver]
    cat x         = x

----------------------------------------------------------------

getOptNames :: GetOptSpec -> (String,String)
getOptNames (Option (c:_) (s:_) _ _) = ('-':[c],'-':'-':s)
getOptNames _                        = error "getOptNames"

resolveOptionString :: [Arg] -> Switch -> [UnknownOpt]
resolveOptionString oargs sw = case lookup sw optionDB of
    Nothing    -> error "resolveOptionString"
    Just gspec -> let (s,l) = getOptNames gspec
                  in checkShort s ++ checkLong l
  where
    checkShort s = filter (==s) oargs
    checkLong  l = filter (l `isPrefixOf`) oargs

optionsToString :: [Option] -> SwitchDB -> [String]
optionsToString opts swdb = concatMap suboption opts
  where
    suboption opt = case lookup (toSwitch opt) swdb of
        Nothing            -> []
        Just None          -> []
        Just (Solo x)      -> [x]
        Just (WithEqArg x) -> [x ++ "=" ++ optionArg opt]
        Just (FollowArg x) -> [x ++ optionArg opt]
