module Types where

import Data.Maybe

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
             | Help
             deriving (Eq,Show)

data CommandSpec = CommandSpec {
    command :: Command
  , commandNames :: [String]
  , document :: String
  , routing :: Route
  , options :: [(Option, Maybe String)]
  }

type CommandDB = [CommandSpec]

----------------------------------------------------------------

data Option = OptNoHarm
            | OptRecursive
            | OptAll
            deriving (Eq,Show)

data OptionSpec = OptionSpec {
    option :: Option
  , optionNames :: [String]
  , optionDesc :: String
  }

type OptionDB  = [OptionSpec]

data Flags = Flags {
    noHarmFlag :: Bool
  , noHarmOption :: Maybe String
  , recursiveFlag :: Bool
  , recursiveOption :: Maybe String
  , allFlag :: Bool
  , allOption :: Maybe String
  }

defaultFlags :: Flags
defaultFlags = Flags {
    noHarmFlag = False
  , noHarmOption = Nothing
  , recursiveFlag = False
  , recursiveOption = Nothing
  , allFlag = False
  , allOption = Nothing
  }

updateFlags :: Option -> Maybe String -> Flags -> Flags
updateFlags OptNoHarm val flg = flg {
    noHarmFlag = True
  , noHarmOption = val
  }
updateFlags OptRecursive val flg = flg {
    recursiveFlag = True
  , recursiveOption = val
  }
updateFlags OptAll val flg = flg {
      allFlag = True
    , allOption = val
    }

flagsToOptions :: Flags -> [String]
flagsToOptions flags = catMaybes [
    noHarmOption flags
  , recursiveOption flags
  , allOption flags
  ]

----------------------------------------------------------------

type FunctionCommand = CommandSpec -> [String] -> Flags -> IO ()

data Route = RouteFunc FunctionCommand
           | RouteProc String [String]

----------------------------------------------------------------
