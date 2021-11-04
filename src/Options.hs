module Options (optionDB, getOptDB) where

import Distribution.Cab
import System.Console.GetOpt

import Types

----------------------------------------------------------------

getOptDB :: GetOptDB
getOptDB = [
    Option ['n'] ["dry-run"]
      (NoArg OptNoharm)
      "Run without destructive operations"
  , Option ['r'] ["recursive"]
      (NoArg OptRecursive)
      "Follow dependencies recursively"
  , Option ['a'] ["all"]
      (NoArg OptAll)
      "Show global packages in addition to user packages"
  , Option ['m'] ["info"]
      (NoArg OptInfo)
      "Show license and author information"
  , Option ['f'] ["flags"]
      (ReqArg OptFlag "<flags>")
      "Specify flags"
  , Option ['t'] ["test"]
      (NoArg OptTest)
      "Enable test"
  , Option ['b'] ["bench"]
      (NoArg OptBench)
      "Enable benchmark"
  , Option ['d'] ["dep-only"]
      (NoArg OptDepsOnly)
      "Target only dependencies"
  , Option ['p'] ["lib-prof"]
      (NoArg OptLibProfile)
      "Enable library profiling"
  , Option ['e'] ["exec-prof"]
      (NoArg OptExecProfile)
      "Enable library profiling"
  , Option ['g'] ["debug"]
      (NoArg OptDebug)
      "Enable debug trace"
  , Option ['j'] ["jobs"]
      (ReqArg OptJobs "<jobs>")
      "Run N jobs"
  , Option ['i'] ["import"]
      (ReqArg OptImport "<dir>:<dir>")
      "Add module import paths"
  , Option ['s'] ["static"]
      (NoArg OptStatic)
      "Create static libraries only"
  , Option ['u'] ["future"]
      (NoArg OptFuture)
      "Show packages with versions ahead of Hackage"
  , Option ['x'] ["newer"]
       (NoArg OptAllowNewer)
      "Allow newer versions"
  , Option ['h'] ["help"]
      (NoArg OptHelp)
      "Show help message"
  ]

optionDB :: OptionDB
optionDB = zip [SwNoharm,SwRecursive,SwAll,SwInfo,SwFlag,SwTest,SwBench,SwDepsOnly,SwLibProfile,SwExecProfile,SwDebug,SwJobs,SwImport,SwStatic,SwFuture,SwAllowNewer] getOptDB
