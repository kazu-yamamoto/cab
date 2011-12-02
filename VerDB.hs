{-# LANGUAGE OverloadedStrings #-}

module VerDB (
    VerDB, getVerDB, lookupLatestVersion, getVerAlist
  ) where

import Control.Applicative
import Control.Arrow (second)
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Enumerator
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Process

----------------------------------------------------------------

type VerInfo = (String, Maybe [Int])

data VerDB = VerDB (Map String [Int])

----------------------------------------------------------------

getVerDB :: IO VerDB
getVerDB = VerDB . M.fromList <$> getVerAlist True

getVerAlist :: Bool -> IO [(String,[Int])]
getVerAlist installedOnly = justOnly <$> verInfos
  where
    script = if installedOnly
             then "cabal list --installed"
             else "cabal list"
    verInfos = infoFromProcess script cabalListParser
    justOnly = map (second fromJust) . filter (isJust . snd)

----------------------------------------------------------------

lookupLatestVersion :: String -> VerDB -> Maybe [Int]
lookupLatestVersion pkgid (VerDB db) = M.lookup pkgid db

----------------------------------------------------------------

cabalListParser :: Iteratee ByteString IO [VerInfo]
cabalListParser = iterParser verinfos

verinfos :: Parser [VerInfo]
verinfos = many1 verinfo

verinfo :: Parser VerInfo
verinfo = do
    name <- string "* " *> nonEols <* endOfLine
    synpsis
    lat <- latestLabel *> latest <* endOfLine
    many skip
    endOfLine
    return (name, lat)
  where
    latestLabel = string "    Default available version: " -- cabal 0.10
              <|> string "    Latest version available: "  -- cabal 0.8
    skip = many1 nonEols *> endOfLine
    synpsis = string "    Synopsis:" *> nonEols *> endOfLine *> more
          <|> return ()
      where
        more = () <$ many (string "     " *> nonEols *> endOfLine)
    latest = Nothing <$ (char '[' *> nonEols)
         <|> Just <$> dotted

dotted :: Parser [Int]
dotted = decimal `sepBy` char '.'

nonEols :: Parser String
nonEols = many1 $ satisfy (notInClass "\r\n")
