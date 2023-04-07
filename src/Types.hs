module Types where

import Distribution.Cab
import System.Console.GetOpt

----------------------------------------------------------------

type Arg = String

----------------------------------------------------------------

data Switch = SwNoharm
            | SwRecursive
            | SwAll
            | SwInfo
            | SwFlag
            | SwTest
            | SwBench
            | SwDepsOnly
            | SwLibProfile
            | SwExecProfile
            | SwDebug
            | SwJobs
            | SwImport
            | SwStatic
            | SwFuture
            | SwAllowNewer
            | SwCleanUp
            deriving (Eq,Show)

----------------------------------------------------------------

data SwitchKind = None | Solo String | WithEqArg String | FollowArg String

type SwitchSpec = (Switch, SwitchKind)
type SwitchDB = [SwitchSpec]

type GetOptSpec = OptDescr Option
type GetOptDB = [GetOptSpec]

type OptionSpec = (Switch,GetOptSpec)
type OptionDB = [OptionSpec]

----------------------------------------------------------------

data Command = Sync
             | Install
             | Uninstall
             | Installed
             | Configure
             | Build
             | Clean
             | Outdated
             | Sdist
             | Upload
             | Unpack
             | Info
             | Deps
             | RevDeps
             | Check
             | GenPaths
             | Search
             | Add
             | Ghci
             | Test
             | Bench
             | Doc
             | Init
             | DocTest
             | Help
             deriving (Eq,Show)

data CommandSpec = CommandSpec {
    command :: Command
  , commandNames :: [String]
  , document :: String
  , routing :: Route
  , switches :: SwitchDB
  , manual  :: Maybe String
  }

type CommandDB = [CommandSpec]

----------------------------------------------------------------

data Route = RouteFunc FunctionCommand
           | RouteCabal [String]
