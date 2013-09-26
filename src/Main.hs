module Main where

import Control.Exception (Handler(..))
import qualified Control.Exception as E (catches)
import Control.Monad (when)
import Data.List (isPrefixOf, intercalate)
import Data.Maybe (isNothing)
import Distribution.Cab
import System.Console.GetOpt (ArgOrder(..), OptDescr(..), getOpt')
import System.Environment (getArgs)
import System.Exit (ExitCode, exitFailure)
import System.IO

import Commands
import Doc
import Help
import Options
import Run
import Types

----------------------------------------------------------------

type UnknownOpt = String
type ParsedArgs = Either [UnknownOpt] ([Arg],[Option])

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
        mcmdspec = commandSpecByName act (commandDB helpCommandAndExit)
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
checkOptions2 opts cmdspec oargs func =
    when (unknowns /= []) $
        func (concatMap (resolveOptionString oargs) unknowns)
  where
    unknowns = unknownOptions opts cmdspec

unknownOptions :: [Option] -> CommandSpec -> [Switch]
unknownOptions opts cmdspec = chk specified supported
  where
    chk [] _     = []
    chk (x:xs) ys
      | x `elem` ys = chk xs ys
      | otherwise   = x : chk xs ys
    specified = map toSwitch opts
    supported = map fst $ switches cmdspec

----------------------------------------------------------------

resolveOptionString :: [Arg] -> Switch -> [UnknownOpt]
resolveOptionString oargs sw = case lookup sw optionDB of
    Nothing    -> error "resolveOptionString"
    Just gspec -> let (s,l) = getOptNames gspec
                  in checkShort s ++ checkLong l
  where
    checkShort s = filter (==s) oargs
    checkLong  l = filter (l `isPrefixOf`) oargs

getOptNames :: GetOptSpec -> (String,String)
getOptNames (Option (c:_) (s:_) _ _) = ('-':[c],'-':'-':s)
getOptNames _                        = error "getOptNames"

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
