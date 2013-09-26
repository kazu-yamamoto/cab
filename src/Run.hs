module Run where

import Control.Monad (void)
import Data.List (intercalate)
import Distribution.Cab
import System.Cmd (system)

import Types

run :: CommandSpec -> [Arg] -> [Option] -> IO ()
run cmdspec params opts = case routing cmdspec of
    RouteFunc func     -> func params opts options
    RouteCabal subargs -> callProcess pro subargs params options
  where
    pro = "cabal"
    sws = switches cmdspec
    options = optionsToString opts sws

optionsToString :: [Option] -> SwitchDB -> [String]
optionsToString opts swdb = concatMap suboption opts
  where
    suboption opt = case lookup (toSwitch opt) swdb of
        Nothing            -> []
        Just None          -> []
        Just (Solo x)      -> [x]
        Just (WithEqArg x) -> [x ++ "=" ++ optionArg opt]
        Just (FollowArg x) -> [x ++ optionArg opt]

callProcess :: String -> [String] -> [Arg] -> [String] -> IO ()
callProcess pro args0 args1 options = void . system $ script
  where
    script = intercalate " " $ pro : args0 ++ cat args1 ++ options
    cat [pkg,ver] = [pkg ++ "-" ++ ver]
    cat x         = x
