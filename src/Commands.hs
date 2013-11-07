module Commands where

import Distribution.Cab

import Types

----------------------------------------------------------------

commandDB :: FunctionCommand -> [CommandSpec]
commandDB help = [
    CommandSpec {
         command = Sync
       , commandNames = ["sync", "update"]
       , document = "Fetch the latest package index"
       , routing = RouteCabal ["update"]
       , switches = []
       , manual = Nothing
       }
  , CommandSpec {
         command = Install
       , commandNames = ["install"]
       , document = "Install packages"
       , routing = RouteCabal ["install"]
       , switches = [(SwNoharm, Solo "--dry-run -v")
                    ,(SwFlag, WithEqArg "--flags")
                    ,(SwTest, Solo "--enable-tests")
                    ,(SwBench, Solo "--enable-benchmarks")
                    ,(SwDepsOnly, Solo "--only-dependencies")
                    ,(SwLibProfile, Solo "--enable-library-profiling --ghc-options=\"-fprof-auto -fprof-cafs\"")
                    ,(SwExecProfile, Solo "--enable-executable-profiling --ghc-options=\"-fprof-auto -fprof-cafs\"")
                    ,(SwJobs, WithEqArg "--jobs")
                    ,(SwStatic, Solo "--disable-shared")
                    ]
       , manual = Just "[<package> [<ver>]]"
       }
  , CommandSpec {
         command = Uninstall
       , commandNames = ["uninstall", "delete", "remove", "unregister"]
       , document = "Uninstall packages"
       , routing = RouteFunc uninstall
       , switches = [(SwNoharm, None)
                    ,(SwRecursive, None)
                    ] -- don't allow SwAll
       , manual = Just "<package> [<ver>]"
       }
  , CommandSpec {
         command = Installed
       , commandNames = ["installed", "list"]
       , document = "List installed packages"
       , routing = RouteFunc installed
       , switches = [(SwAll, None)
                    ,(SwRecursive, None)
                    ,(SwInfo, None)
                    ]
       , manual = Nothing
       }
  , CommandSpec {
         command = Configure
       , commandNames = ["configure", "conf"]
       , document = "Configure a cabal package"
       , routing = RouteCabal ["configure"]
       , switches = [(SwFlag, WithEqArg "--flags")
                    ,(SwTest, Solo "--enable-tests")
                    ,(SwBench, Solo "--enable-benchmarks")
                    ,(SwLibProfile, Solo "--enable-library-profiling --ghc-options=\"-fprof-auto -fprof-cafs\"")
                    ,(SwExecProfile, Solo "--enable-executable-profiling --ghc-options=\"-fprof-auto -fprof-cafs\"")
                    ,(SwStatic, Solo "--disable-shared")
                    ]
       , manual = Nothing
       }
  , CommandSpec {
         command = Build
       , commandNames = ["build"]
       , document = "Build a cabal package"
       , routing = RouteCabal ["build"]
       , switches = [(SwJobs, WithEqArg "--jobs")]
       , manual = Nothing
       }
  , CommandSpec {
         command = Clean
       , commandNames = ["clean"]
       , document = "Clean up a build directory"
       , routing = RouteCabal ["clean"]
       , switches = []
       , manual = Nothing
       }
  , CommandSpec {
         command = Outdated
       , commandNames = ["outdated"]
       , document = "Display outdated packages"
       , routing = RouteFunc outdated
       , switches = [(SwAll, None)]
       , manual = Nothing
       }
  , CommandSpec {
         command = Info
       , commandNames = ["info"]
       , document = "Display information of a package"
       , routing = RouteCabal ["info"]
       , switches = []
       , manual = Just "<package> [<ver>]"
       }
  , CommandSpec {
         command = Sdist
       , commandNames = ["sdist", "pack"]
       , document = "Make tar.gz for source distribution"
       , routing = RouteCabal ["sdist"]
       , switches = []
       , manual = Nothing
       }
  , CommandSpec {
         command = Upload
       , commandNames = ["upload", "up"]
       , document = "Uploading tar.gz to HackageDB"
       , routing = RouteCabal ["upload"]
       , switches = [(SwNoharm, Solo "-c")]
       , manual = Nothing
       }
  , CommandSpec {
         command = Unpack
       , commandNames = ["unpack"]
       , document = "Untar a package in the current directory"
       , routing = RouteCabal ["unpack"]
       , switches = []
       , manual = Just "<package> [<ver>]"
       }
  , CommandSpec {
         command = Deps
       , commandNames = ["deps"]
       , document = "Show dependencies of this package"
       , routing = RouteFunc deps
       , switches = [(SwRecursive, None)
                    ,(SwAll, None)
                    ,(SwInfo, None)
                    ]
       , manual = Just "<package> [<ver>]"
       }
  , CommandSpec {
         command = RevDeps
       , commandNames = ["revdeps", "dependents"]
       , document = "Show reverse dependencies of this package"
       , routing = RouteFunc revdeps
       , switches = [(SwRecursive, None)
                    ,(SwAll, None)
                    ,(SwInfo, None)
                    ]
       , manual = Just "<package> [<ver>]"
       }
  , CommandSpec {
         command = Check
       , commandNames = ["check"]
       , document = "Check consistency of packages"
       , routing = RouteFunc check
       , switches = []
       , manual = Nothing
       }
  , CommandSpec {
         command = GenPaths
       , commandNames = ["genpaths", "genpath"]
       , document = "Generate Paths_<pkg>.hs"
       , routing = RouteFunc genpaths
       , switches = []
       , manual = Nothing
       }
  , CommandSpec {
         command = Search
       , commandNames = ["search"]
       , document = "Search available packages by package name"
       , routing = RouteFunc search
       , switches = []
       , manual = Just "<key>"
       }
  , CommandSpec {
         command = Add
       , commandNames = ["add", "add-source"]
       , document = "Add a source directory"
       , routing = RouteFunc add
       , switches = []
       , manual = Just "<source>"
       }
  , CommandSpec {
         command = Test
       , commandNames = ["test"]
       , document = "Run tests"
       , routing = RouteCabal ["test"]
       , switches = []
       , manual = Just "[testsuite]"
       }
  , CommandSpec {
         command = Bench
       , commandNames = ["bench"]
       , document = "Run benchmarks"
       , routing = RouteCabal ["bench"]
       , switches = []
       , manual = Nothing
       }
  , CommandSpec {
         command = Doc
       , commandNames = ["doc", "haddock", "man"]
       , document = "Generate manuals"
       , routing = RouteCabal ["haddock", "--hyperlink-source"]
       , switches = []
       , manual = Nothing
       }
  , CommandSpec {
         command = Ghci
       , commandNames = ["ghci", "repl"]
       , document = "Run GHCi (with a sandbox)"
       , routing = RouteFunc ghci
       , switches = [(SwImport, FollowArg "-i")]
       , manual = Nothing
       }
  , CommandSpec {
         command = Init
       , commandNames = ["init"]
       , document = "Initialize a sandbox"
       , routing = RouteFunc initSandbox
       , switches = []
       , manual = Nothing
       }
  , CommandSpec {
         command = Help
       , commandNames = ["help"]
       , document = "Display the help message of the command"
       , routing = RouteFunc help
       , switches = []
       , manual = Just "[<command>]"
       }
  ]
