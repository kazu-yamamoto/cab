module Distribution.Cab.Version (
    Ver
  , toVer
  , verToString
  , version
  , versionToString
  ) where

import Distribution.Cab.Utils
import Distribution.Version (Version(..))

newtype Ver = Ver [Int] deriving Eq

toVer :: [Int] -> Ver
toVer is = Ver is

verToString :: Ver -> String
verToString (Ver ver) = toDotted ver

version :: Version -> Ver
version = Ver . versionBranch

versionToString :: Version -> String
versionToString = verToString . version


