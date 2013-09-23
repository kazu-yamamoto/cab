module Distribution.Cab.Version (
    Ver
  , toVer
  , verToString
  , version
  , versionToString
  ) where

import Distribution.Cab.Utils
import Distribution.Version (Version(..))

-- | Package version.
newtype Ver = Ver [Int] deriving (Eq,Show)

-- | Creating 'Ver'.
--
-- >>> toVer [1,2,3]
-- Ver [1,2,3]
toVer :: [Int] -> Ver
toVer is = Ver is

-- | From 'Version' to 'String'
--
-- >>> verToString $ toVer [1,2,3]
-- "1.2.3"
verToString :: Ver -> String
verToString (Ver ver) = toDotted ver

-- | From 'Version' in Cabal to 'Ver'.
--
-- >>> version $ Version [1,2,3] []
-- Ver [1,2,3]
version :: Version -> Ver
version = Ver . versionBranch

-- | From 'Version' in Cabal to 'String'.
--
-- >>> versionToString $ Version [1,2,3] []
-- "1.2.3"
versionToString :: Version -> String
versionToString = verToString . version


