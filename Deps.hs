module Deps where

import Types
import PkgDB

deps :: FunctionCommand
deps _ [] _ = return () -- FIXME
deps _ (pkgnm:_) _ = do
    db <- getPkgDB
    mapM_ (printPkg pkgnm db) $ lookupByName pkgnm db
