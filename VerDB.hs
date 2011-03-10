{-# LANGUAGE OverloadedStrings #-}

module VerDB (
    VerDB, getVerDB, lookupLatestVersion
  ) where

import Control.Applicative hiding (many)
import Data.Attoparsec.Char8
import Data.Attoparsec.Enumerator
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Distribution.Package (PackageId, PackageIdentifier(..), PackageName(..))
import Distribution.Version (Version(..))
import Process

----------------------------------------------------------------

data VerInfo = VerInfo {
    packageName :: String
  , currentVersion :: [Int]
  , latestVersion :: Maybe [Int]
  } deriving Show

data VerDB = VerDB (Map PackageId [Int])

----------------------------------------------------------------

getVerDB :: IO VerDB
getVerDB = do
    verInfos <- infoFromProcess "cabal list --installed" cabalListParser
    let verInfos' = filter (isJust . latestVersion) verInfos
        pkgIDs = map toPackageId verInfos'
        vers = map (fromJust . latestVersion) verInfos'
        verDB = M.fromList $ zip pkgIDs vers
    return (VerDB verDB)

toPackageId :: VerInfo -> PackageId
toPackageId veri = PackageIdentifier {
    pkgName = PackageName (packageName veri)
  , pkgVersion = Version {
        versionBranch = currentVersion veri
      , versionTags = []
      }
  }

----------------------------------------------------------------

lookupLatestVersion :: PackageId -> VerDB -> Maybe [Int]
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
    lat <- string "    Latest version available: " *> latest <* endOfLine
    cur <- string "    Latest version installed: " *> dotted <* endOfLine
    many skip
    endOfLine
    return $ VerInfo {
        packageName = name
      , currentVersion = cur
      , latestVersion = lat
      }
  where
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
nonEols = many1 $ satisfy (notInClass "\n")
