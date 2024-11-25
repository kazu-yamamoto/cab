{-# LANGUAGE CPP #-}

module Distribution.Cab.Version (
    Ver,
    toVer,
    toVersion,
    verToString,
    version,
    versionToString,
) where

import Distribution.Cab.Utils
import Distribution.Version

-- | Package version.
newtype Ver = Ver [Int] deriving (Eq, Ord, Read, Show)

-- | Creating 'Ver'.
--
-- >>> toVer [1,2,3]
-- Ver [1,2,3]
toVer :: [Int] -> Ver
toVer is = Ver is

-- | Creating 'Version' in Cabal.
toVersion :: [Int] -> Version
#if MIN_VERSION_Cabal(2,0,0)
toVersion is = mkVersion is
#else
toVersion is = Version is []
#endif

-- | From 'Version' to 'String'
--
-- >>> verToString $ toVer [1,2,3]
-- "1.2.3"
verToString :: Ver -> String
verToString (Ver ver) = toDotted ver

-- | From 'Version' in Cabal to 'Ver'.
--
-- >>> version $ toVersion [1,2,3]
-- Ver [1,2,3]
version :: Version -> Ver
#if MIN_VERSION_Cabal(2,0,0)
version = Ver . versionNumbers
#else
version = Ver . versionBranch
#endif

-- | From 'Version' in Cabal to 'String'.
--
-- >>> versionToString $ toVersion [1,2,3]
-- "1.2.3"
versionToString :: Version -> String
versionToString = verToString . version
