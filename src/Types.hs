module Types where

import Distribution.Cab
import System.Console.GetOpt

type Arg = String
type UnknownOpt = String
type ParsedArgs = Either [UnknownOpt] ([Arg],[Option])

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
            | SwJobs
            | SwImport
            deriving (Eq,Show)

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
toSwitch _              = error "toSwitch"

----------------------------------------------------------------

optionArg :: Option -> String
optionArg (OptFlag   str) = str
optionArg (OptJobs   str) = str
optionArg (OptImport str) = str
optionArg _               = ""

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
