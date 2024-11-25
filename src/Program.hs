module Program (
    version,
    showVersion,
    programName,
    description,
) where

import Data.Version
import Paths_cab

programName :: String
programName = "cab"

description :: String
description = "A maintenance command of Haskell cabal packages"
