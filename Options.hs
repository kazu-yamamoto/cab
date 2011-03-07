module Options where

import System.Console.GetOpt

data Options = Options {
    help :: Bool
}

defaultOptions :: Options
defaultOptions = Options {
    help = False
  }

argSpec :: [OptDescr (Options -> Options)]
argSpec = [ Option "h" ["help"]
            (NoArg (\opts -> opts { help = True }))
            "print help messages"
          ]

