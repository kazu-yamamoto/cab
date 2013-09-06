module Distribution.Cab.GHCVer where

import Control.Applicative
import Distribution.Simple.Program (ghcProgram)
import Distribution.Simple.Program.Types (programName, programFindVersion)
import Distribution.Verbosity (silent)
import Distribution.Version (versionBranch)

ghcVersion :: IO Int
ghcVersion = toInt <$> ghcVer
  where
    ghcVer = programFindVersion ghcProgram silent (programName ghcProgram)
    toInt Nothing = 0
    toInt (Just v)
      | length vs < 2 = 0
      | otherwise     = (vs !! 0) * 100 + (vs !! 1)
      where
        vs = versionBranch v
