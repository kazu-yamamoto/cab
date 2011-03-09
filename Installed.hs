module Installed where

import Control.Applicative
import PkgDB
import Types

installed :: FunctionCommand
installed _ _ _ = do
    pkgs <- toPkgList <$> getPkgDB <*> makeUserOnly
    mapM_ putStrLn $ map pkgName' pkgs
