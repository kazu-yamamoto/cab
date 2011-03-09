module PkgDB where

import Distribution.Version
-- import Distribution.Compiler
import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Simple.Compiler
-- import Distribution.Simple.Configure hiding (configure, getInstalledPackages)
import Distribution.Simple.GHC
import Distribution.Simple.PackageIndex
-- import Distribution.Simple.Program
import Distribution.Simple.Program.Db
import Distribution.Verbosity
import Data.List

getPkgDB :: IO PackageIndex
getPkgDB = do
    (_,pro) <- configure normal Nothing Nothing emptyProgramDb
    getInstalledPackages normal [GlobalPackageDB,UserPackageDB] pro

getPkgNames :: PackageIndex -> (InstalledPackageInfo -> Bool) -> [String]
getPkgNames db prd = map toName $ map sourcePackageId $ filter prd $ allPackages db

toName :: PackageId -> String
toName pid = name ++ " " ++ ver
  where
      PackageName name = pkgName pid
      is = versionBranch . pkgVersion $ pid
      ver = concat $ intersperse "." $ map show is

userOnly :: FilePath -> InstalledPackageInfo -> Bool
userOnly dir pkgi = dir `isPrefixOf` (head $ libraryDirs pkgi)
