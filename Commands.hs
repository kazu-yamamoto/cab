module Commands where

data Command = Sync
             | Install
             | Uninstall
             | Installed
             | Configure
             | Build
             | Info
             | Help
             deriving (Eq,Show)

data Route = RouteFunc ([String] -> IO ())
           | RouteProc String [String]

data CommandSpec = CommandSpec {
    command :: Command
  , commandNames :: [String]
  , document :: String
  , routing :: Route
  }

type CommandDB = [CommandSpec]

commandDB :: CommandDB
commandDB = [
    CommandSpec {
         command = Sync
       , commandNames = ["sync", "update"]
       , document = "Fetchinwg the latest package index"
       , routing = RouteProc "cabal" ["update"]
       }
  , CommandSpec {
         command = Install
       , commandNames = ["install"]
       , document = "Install packages"
       , routing = RouteProc "cabal" ["install"]
       }
  , CommandSpec {
         command = Uninstall
       , commandNames = ["uninstall"]
       , document = "Uninstalling packages"
       , routing = RouteProc "ghc-pkg" ["unregister"]
       }
  , CommandSpec {
         command = Installed
       , commandNames = ["installed"]
       , document = "Listing installed packages"
       , routing = RouteProc "ghc-pkg" ["list"]
       }
  , CommandSpec {
         command = Configure
       , commandNames = ["configure"]
       , document = "Configuring a cabal package"
       , routing = RouteProc "cabal" ["configure"]
       }
  , CommandSpec {
         command = Build
       , commandNames = ["build"]
       , document = "Building a cabal package"
       , routing = RouteProc "cabal" ["build"]
       }
  , CommandSpec {
         command = Info
       , commandNames = ["info"]
       , document = "Display information of a package"
       , routing = RouteProc "cabal" ["info"]
       }
  , CommandSpec {
         command = Help
       , commandNames = ["help"]
       , document = undefined
       , routing = undefined
       }
  ]

getCommand :: String -> CommandDB -> Command
getCommand _ [] = Help
getCommand x (ent:ents)
  | x `elem` commandNames ent = command ent
  | otherwise   = getCommand x ents

commandSpecByCommand :: Command -> CommandDB -> Maybe CommandSpec
commandSpecByCommand _ [] = Nothing
commandSpecByCommand x (ent:ents)
    | x == command ent = Just ent
    | otherwise        = commandSpecByCommand x ents
