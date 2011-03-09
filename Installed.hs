module Installed where

import PkgDB
import System.Directory
import Types

installed :: FunctionCommand
installed _ _ _ = do
    userDirPref <- getAppUserDataDirectory ""
    db <- getPkgDB
    mapM_ putStrLn $ getPkgNames db (userOnly userDirPref)
