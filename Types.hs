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
            | SwInfo
            | SwSandbox
            | SwFlag
            | SwTest
            deriving (Eq,Show)

data Option = OptNoharm
            | OptRecursive
            | OptAll
            | OptInfo
            | OptSandbox String
            | OptFlag String
            | OptTest
            | OptHelp
            deriving (Eq,Show)

toSwitch :: Option -> Switch
toSwitch OptNoharm      = SwNoharm
toSwitch OptRecursive   = SwRecursive
toSwitch OptAll         = SwAll
toSwitch OptInfo        = SwInfo
toSwitch (OptSandbox _) = SwSandbox
toSwitch (OptFlag _)    = SwFlag
toSwitch OptTest        = SwTest
toSwitch _              = error "toSwitch"

getSandbox :: [Option] -> Maybe FilePath
getSandbox = getValue (\x -> toSwitch x == SwSandbox)

getFlag :: [Option] -> Maybe FilePath
getFlag = getValue (\x -> toSwitch x == SwFlag)

getValue :: (Option -> Bool) -> [Option] -> Maybe FilePath
getValue p opts = case find p opts of
    Nothing                -> Nothing
    Just (OptSandbox path) -> Just path
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
             | Env
             | Add
             | Ghci
             | Test
             | Doc
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
           | RouteCabal [String]

cabalCommand :: [Option] -> String
cabalCommand opts
    | SwSandbox `elem` map toSwitch opts = "cabal-dev"
    | otherwise                          = "cabal"

----------------------------------------------------------------

cabEnvVar :: String
cabEnvVar = "CAB_SANDBOX_PATH"

ghcEnvVar :: String
ghcEnvVar = "GHC_PACKAGE_PATH"
