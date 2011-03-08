{-# LANGUAGE OverloadedStrings #-}

module Outdated where

import Control.Applicative hiding (many)
import Data.Attoparsec.Char8
import Data.Attoparsec.Enumerator
import Data.ByteString (ByteString)
import Data.Enumerator (Iteratee, run_, ($$))
import Data.Enumerator.Binary (enumHandle)
import System.Process
import System.IO
import Types

outdated :: FunctionCommand
outdated _ _ _ = do
    (Nothing, Just hdl, Nothing, _) <- createProcess proSpec
    hSetEncoding hdl latin1
    olds <- run_ (enumHandle 4096 hdl $$ outdatedParser)
    mapM_ printOld olds
  where
    proSpec = CreateProcess {
        cmdspec = ShellCommand "cabal list --installed"
      , cwd = Nothing
      , env = Nothing
      , std_in = Inherit
      , std_out = CreatePipe
      , std_err = Inherit
      , close_fds = True
}

data OldPkg = OldPkg String String String deriving Show

printOld :: OldPkg -> IO ()
printOld (OldPkg name cur new) = putStrLn $ name ++ " " ++ cur ++ " < " ++ new

outdatedParser :: Iteratee ByteString IO [OldPkg]
outdatedParser = iterParser oldpkgs

oldpkgs :: Parser [OldPkg]
oldpkgs = filter (\(OldPkg _ x y) -> x /= y) <$> many1 oldpkg

oldpkg :: Parser OldPkg
oldpkg = do
    name <- string "* " *> nonEols <* endOfLine
    synpsis
    new <- string "    Latest version available: " *> nonEols <* endOfLine
    cur <- string "    Latest version installed: " *> nonEols <* endOfLine
    many skip
    endOfLine
    return $ OldPkg name cur new
  where
    nonEols = many1 $ satisfy (notInClass "\n")
    skip = many1 nonEols *> endOfLine
    synpsis = string "    Synopsis:" *> nonEols *> endOfLine *> more
          <|> return ()
      where
        more = () <$ many (string "     " *> nonEols *> endOfLine)
