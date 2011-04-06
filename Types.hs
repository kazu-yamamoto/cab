module Types where

import Data.List
import System.Console.GetOpt

type Arg = String
type UnknownOpt = String
type ParsedArgs = Either [UnknownOpt] ([Arg],[Option])

----------------------------------------------------------------

data Switch = SwNoharm
            | SwRecursive
            | SwAll
            | SwSandbox
            deriving (Eq,Show)

data Option = OptNoharm
            | OptRecursive
            | OptAll
            | OptSandbox String
            deriving (Eq,Show)

toSwitch :: Option -> Switch
toSwitch OptNoharm      = SwNoharm
toSwitch OptRecursive   = SwRecursive
toSwitch OptAll         = SwAll
toSwitch (OptSandbox _) = SwSandbox

getSandbox :: [Option] -> Maybe FilePath
getSandbox opts = case find isSandbox opts of
    Nothing                -> Nothing
    Just (OptSandbox path) -> Just path
    _                      -> error "getSandbox"
  where
    isSandbox (OptSandbox _) = True
    isSandbox _              = False

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
             | Unpack
             | Info
             | Deps
             | RevDeps
             | Check
             | Search
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

type FunctionCommand = CommandSpec -> [String] -> [Option] -> IO ()

data Route = RouteFunc FunctionCommand
           | RouteProc String [String]

----------------------------------------------------------------
