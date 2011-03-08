{-# LANGUAGE OverloadedStrings #-}

module Outdated where

import Control.Applicative hiding (many)
import Data.Attoparsec.Char8
import Data.Attoparsec.Enumerator
import Data.ByteString (ByteString)
import Data.Enumerator (Iteratee, run_, ($$))
import Data.Enumerator.Binary (enumHandle)
import qualified Data.Map as M
import Data.Maybe
import Distribution.InstalledPackageInfo
import Distribution.InstalledPackageInfo.Binary
import Distribution.Package
import System.FilePath
import System.IO
import System.Process
import Types

outdated :: FunctionCommand
outdated _ _ _ = do
    olds <- infoFromCommand "cabal list --installed" cabalListParser
            >>= userOnly
    mapM_ printOld olds

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

data OldPkg = OldPkg String String String deriving Show

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

----------------------------------------------------------------

type PackageInfo = InstalledPackageInfo_ String

getUserPackageInfo :: FilePath -> IO [PackageInfo]
getUserPackageInfo file = readBinPackageDB file :: IO [PackageInfo]

getUserCacheFilePath :: IO FilePath
getUserCacheFilePath =
    (</> "package.cache") <$> infoFromCommand "ghc-pkg list" ghcPkgListParser

ghcPkgListParser :: Iteratee ByteString IO FilePath
ghcPkgListParser = iterParser $ do
    nonEols *> endOfLine
    many1 (char ' ' *> nonEols *> endOfLine)
    endOfLine
    nonColons <* char ':' <* endOfLine
  where
    nonColons = many1 $ satisfy (notInClass ":\n")

userOnly :: [OldPkg] -> IO [OldPkg]
userOnly olds = do
    m <- toMap <$> (getUserCacheFilePath >>= getUserPackageInfo)
    return $ filter (\(OldPkg x _ _) -> isJust (M.lookup x m)) olds
  where
    toMap ps = M.fromList $ zip (map toString ps) ps
    toString = fromPackageName . pkgName . sourcePackageId
    fromPackageName (PackageName x) = x
