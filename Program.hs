module Program (
    version, showVersion
  , programName, description
  ) where

import Paths_cab
import Data.Version

programName :: String
programName = "cab"

description :: String
description = "A maintenance command of Haskell cabal packages"
