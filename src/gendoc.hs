-- runghc -- -package-db --ghc-arg=../.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d gendoc.hs

module Main where

import Commands
import Doc
import Options
import Types

main :: IO ()
main = do
    putStrLn "The following commands are provided:"
    putStr "\n"
    putStrLn "?cab"
    putStrLn "!Display the help message."
    -- undefined prevents to import "Path_cab.hs"
    mapM_ prCmdSpec (commandDB undefined)
    putStr "\n"
    putStrLn "The following options are provided:"
    putStr "\n"
    mapM_ prOptSpec optionDB

prCmdSpec :: CommandSpec -> IO ()
prCmdSpec cmdspec = do
    putStrLn $ "?cab " ++ escape usage
    putStrLn $ "!" ++ doc ++ "." ++ aliases
  where
    (usage,doc,alias) = usageDocAlias cmdspec
    aliases
      | alias == "" = ""
      | otherwise   = "\\<br /\\>Command aliases: " ++ escape alias

prOptSpec :: OptionSpec -> IO ()
prOptSpec spec = do
    putStrLn $ "?" ++ option
    putStrLn $ "!" ++ doc
  where
    (option, doc) = optionDoc spec

escape :: String -> String
escape [] = []
escape ('[':rest) = "\\[" ++ escape rest
escape (']':rest) = "\\]" ++ escape rest
escape (c:rest)   = c : escape rest
