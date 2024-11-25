module Doc where

import Data.List (intercalate)
import System.Console.GetOpt (ArgDescr (..), OptDescr (..))

import Options
import Types

commandSpecByName :: String -> CommandDB -> Maybe CommandSpec
commandSpecByName _ [] = Nothing
commandSpecByName x (ent : ents)
    | x `elem` commandNames ent = Just ent
    | otherwise = commandSpecByName x ents

----------------------------------------------------------------

usageDocAlias :: CommandSpec -> (String, String, String)
usageDocAlias cmdspec = (usage, doc, alias)
  where
    usage = cmd ++ " " ++ showOptions ++ showArgs
    doc = document cmdspec
    alias = showAliases cmdspec
    cmd : _ = commandNames cmdspec
    options = opts cmdspec
    showOptions
        | null options = ""
        | otherwise =
            "["
                ++ intercalate "] [" (concatMap (masterOption optionDB) (opts cmdspec))
                ++ "]"
    showArgs = maybe "" (" " ++) $ manual cmdspec
    opts = map fst . switches
    masterOption [] _ = []
    masterOption (spec : specs) o
        | fst spec == o = optionName spec : masterOption specs o
        | otherwise = masterOption specs o
    showAliases = intercalate ", " . tail . commandNames

----------------------------------------------------------------

optionDoc :: OptionSpec -> (String, String)
optionDoc spec = (key, doc)
  where
    key = intercalate ", " . reverse . optionNames $ spec
    doc = optionDesc spec

optionName :: OptionSpec -> String
optionName (_, Option (c : _) _ (ReqArg _ arg) _) = '-' : c : ' ' : arg
optionName (_, Option (c : _) _ _ _) = '-' : [c]
optionName _ = ""

optionNames :: OptionSpec -> [String]
optionNames (_, Option (c : _) (s : _) _ _) = ['-' : [c], '-' : '-' : s]
optionNames _ = []

optionDesc :: OptionSpec -> String
optionDesc (_, Option _ _ _ desc) = desc
