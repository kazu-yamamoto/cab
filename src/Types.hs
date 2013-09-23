module Types where

import Data.List
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
            deriving (Eq,Show)

toSwitch :: Option -> Switch
toSwitch OptNoharm      = SwNoharm
toSwitch OptRecursive   = SwRecursive
toSwitch OptAll         = SwAll
toSwitch OptInfo        = SwInfo
toSwitch (OptFlag _)    = SwFlag
toSwitch OptTest        = SwTest
toSwitch OptBench       = SwBench
toSwitch _              = error "toSwitch"

getFlag :: [Option] -> Maybe FilePath
getFlag = getValue (\x -> toSwitch x == SwFlag)

getValue :: (Option -> Bool) -> [Option] -> Maybe FilePath
getValue p opts = case find p opts of
    Nothing                -> Nothing
    _                      -> error "getSandbox"

type SwitchSpec = (Switch, Maybe String)
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
