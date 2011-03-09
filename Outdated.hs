{-# LANGUAGE OverloadedStrings #-}

module Outdated where

import Control.Applicative hiding (many)
import Control.Monad
import Data.Attoparsec.Char8
import Data.Attoparsec.Enumerator
import Data.ByteString (ByteString)
import Data.Enumerator (Iteratee, run_, ($$))
import Data.Enumerator.Binary (enumHandle)
import qualified Data.Map as M
import System.IO
import System.Process
import Types

import PkgDB

outdated :: FunctionCommand
outdated _ _ _ = do
    pkgs <- toPkgList <$> getPkgDB <*> makeUserOnly
    vs <- versionInfos
    let m = M.fromList $ zip (map toPackageId vs) (map latestVersion vs)
    forM_ pkgs $ \p -> do
        case M.lookup (pkgSrcId p) m of
            Nothing -> return ()
            Just ver -> if versionToString (pkgVersion' p) /= ver
               then putStrLn $ pkgName' p ++ " < " ++ ver
               else return ()
 where
    latestVersion (OldPkg _ _ lver) = lver

versionInfos :: IO [OldPkg]
versionInfos = infoFromCommand "cabal list --installed" cabalListParser

----------------------------------------------------------------

infoFromCommand :: String -> Iteratee ByteString IO a -> IO a
infoFromCommand shellCommand parser = do
    (Nothing, Just hdl, Nothing, _) <- createProcess proSpec
    hSetEncoding hdl latin1
    run_ (enumHandle 4096 hdl $$ parser)
  where
    proSpec = CreateProcess {
        cmdspec = ShellCommand shellCommand
      , cwd = Nothing
      , env = Nothing
      , std_in = Inherit
      , std_out = CreatePipe
      , std_err = Inherit
      , close_fds = True
      }

----------------------------------------------------------------

printOld :: OldPkg -> IO ()
printOld (OldPkg name cur new) = putStrLn $ name ++ " " ++ cur ++ " < " ++ new

cabalListParser :: Iteratee ByteString IO [OldPkg]
cabalListParser = iterParser oldpkgs

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
    skip = many1 nonEols *> endOfLine
    synpsis = string "    Synopsis:" *> nonEols *> endOfLine *> more
          <|> return ()
      where
        more = () <$ many (string "     " *> nonEols *> endOfLine)

nonEols :: Parser String
nonEols = many1 $ satisfy (notInClass "\n")
