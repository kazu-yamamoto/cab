{-# LANGUAGE OverloadedStrings #-}

module Distribution.Cab.VerDB (
  -- * Types
    VerDB
  , HowToObtain(..)
  -- * Creating
  , getVerDB
  , toList
  ) where

import Control.Applicative
import Control.Arrow (second)
import Data.Attoparsec.ByteString.Char8
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Process
import Data.Maybe
import Distribution.Cab.Version

----------------------------------------------------------------

type VerInfo = (String, Maybe [Int])

newtype VerDB = VerDB [(String,Ver)]

data HowToObtain = InstalledOnly | AllRegistered

----------------------------------------------------------------

getVerDB :: HowToObtain -> IO VerDB
getVerDB how = VerDB . justOnly <$> verInfos
  where
    script = case how of
        InstalledOnly -> "cabal list --installed"
        AllRegistered -> "cabal list"
    verInfos = runResourceT $ sourceCmd script $$ cabalListParser
    justOnly = map (second (toVer . fromJust)) . filter (isJust . snd)
    cabalListParser = sinkParser verinfos

----------------------------------------------------------------

toList :: VerDB -> [(String, Ver)]
toList (VerDB db) = db

----------------------------------------------------------------

verinfos :: Parser [VerInfo]
verinfos = many1 verinfo

verinfo :: Parser VerInfo
verinfo = do
    name <- string "* " *> nonEols <* endOfLine
    synpsis
    lat <- latestLabel *> latest <* endOfLine
    _ <- many skip
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
