module Run (run, toSwitch) where

import Data.List (intercalate)
import Distribution.Cab
import System.Process (callCommand)

import Types

----------------------------------------------------------------

toSwitch :: Option -> Switch
toSwitch OptNoharm      = SwNoharm
toSwitch OptRecursive   = SwRecursive
toSwitch OptAll         = SwAll
toSwitch OptInfo        = SwInfo
toSwitch (OptFlag _)    = SwFlag
toSwitch OptTest        = SwTest
toSwitch OptBench       = SwBench
toSwitch OptDepsOnly    = SwDepsOnly
toSwitch OptLibProfile  = SwLibProfile
toSwitch OptExecProfile = SwExecProfile
toSwitch (OptJobs _)    = SwJobs
toSwitch (OptImport _)  = SwImport
toSwitch OptStatic      = SwStatic
toSwitch OptFuture      = SwFuture
toSwitch _              = error "toSwitch"

----------------------------------------------------------------

optionArg :: Option -> String
optionArg (OptFlag   str) = str
optionArg (OptJobs   str) = str
optionArg (OptImport str) = str
optionArg _               = ""

optionsToString :: [Option] -> SwitchDB -> [String]
optionsToString opts swdb = concatMap suboption opts
  where
    suboption opt = case lookup (toSwitch opt) swdb of
        Nothing            -> []
        Just None          -> []
        Just (Solo x)      -> [x]
        Just (WithEqArg x) -> [x ++ "=" ++ optionArg opt]
        Just (FollowArg x) -> [x ++ optionArg opt]

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
callProcess pro args0 args1 options = callCommand script
  where
    script = intercalate " " $ pro : args0 ++ cat args1 ++ options
    cat [pkg,ver] = [pkg ++ "-" ++ ver]
    cat x         = x
