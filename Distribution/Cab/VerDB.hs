{-# LANGUAGE OverloadedStrings #-}

module Distribution.Cab.VerDB (
  -- * Types
    PkgName
  , VerDB
  , HowToObtain(..)
  -- * Creating
  , getVerDB
  -- * Converting
  , toList
  , toMap
  ) where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Attoparsec.ByteString.Char8
import Data.Conduit.Attoparsec
import Data.Conduit.Process
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Distribution.Cab.Version

----------------------------------------------------------------

type PkgName = String

type VerInfo = (PkgName, Maybe [Int])

newtype VerDB = VerDB [(PkgName,Ver)] deriving (Eq, Show)

data HowToObtain = InstalledOnly | AllRegistered

----------------------------------------------------------------

getVerDB :: HowToObtain -> IO VerDB
getVerDB how = VerDB . justOnly <$> verInfos
  where
    script = case how of
        InstalledOnly -> "cabal list --installed"
        AllRegistered -> "cabal list"
    verInfos = runResourceT $ sourceCmdWithConsumer script cabalListParser
    justOnly = map (second (toVer . fromJust)) . filter (isJust . snd) . snd
    cabalListParser = sinkParser verinfos

----------------------------------------------------------------

-- | Converting 'VerDB' to alist.
--
-- >>> db <- getVerDB InstalledOnly
-- >>> elem "base" . map fst . toList $ db
-- True
toList :: VerDB -> [(PkgName, Ver)]
toList (VerDB alist) = alist

-- | Converting 'VerDB' to 'Map'.
toMap :: VerDB -> Map PkgName Ver
toMap (VerDB alist) = M.fromList alist

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
