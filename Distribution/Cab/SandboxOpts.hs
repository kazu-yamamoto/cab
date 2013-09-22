module Distribution.Cab.SandboxOpts (getSandboxOpts) where

import Control.Applicative
import Distribution.Simple.Program (ghcProgram)
import Distribution.Simple.Program.Types (programName, programFindVersion)
import Distribution.Verbosity (silent)
import Distribution.Version (versionBranch)

import Distribution.Cab.PkgDB
import Distribution.Cab.Sandbox

getSandboxOpts :: IO String
getSandboxOpts = do
    mpath <- getSandbox
    case mpath of
        Nothing   -> return ""
        Just path -> do
            ghcver <- ghcVersion
            pkgConf <- getPackageConf path
            let pkgOpt | ghcver >= 706 = "-package-db "
                       | otherwise     = "-package-conf "
            return $ pkgOpt ++ pkgConf ++ " "

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
