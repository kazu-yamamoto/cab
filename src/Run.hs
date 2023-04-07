{-# LANGUAGE CPP #-}
module Run (run, toSwitch) where

import Data.List (intercalate)
import Distribution.Cab
#if MIN_VERSION_process(1,2,0)
import System.Process (callCommand)
#else
import Control.Monad (void)
import System.Cmd (system)
#endif

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
toSwitch OptDebug       = SwDebug
toSwitch (OptJobs _)    = SwJobs
toSwitch (OptImport _)  = SwImport
toSwitch OptStatic      = SwStatic
toSwitch OptFuture      = SwFuture
toSwitch OptAllowNewer  = SwAllowNewer
toSwitch OptCleanUp     = SwCleanUp
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
callProcess pro args0 args1 options = systemCommand script
  where
#if MIN_VERSION_process(1,2,0)
    systemCommand = callCommand
#else
    systemCommand = void . system
#endif

    script = intercalate " " $ pro : args0 ++ cat args1 ++ options
    cat [pkg,ver] = [pkg ++ "-" ++ ver]
    cat x         = x
