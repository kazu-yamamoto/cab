{-# LANGUAGE CPP #-}

module Process (
    Iteratee, infoFromProcess
  ) where

import Data.Enumerator (Iteratee, run_, ($$))
import Data.Enumerator.Binary (enumHandle)
import System.Process
import System.IO
import Data.ByteString (ByteString)

infoFromProcess :: String -> Iteratee ByteString IO a -> IO a
infoFromProcess shellCommand parser = do
    (Nothing, Just hdl, Nothing, _) <- createProcess proSpec
    hSetEncoding hdl latin1
    run_ (enumHandle 4096 hdl $$ parser)
  where
    proSpec = CreateProcess {
        cmdspec = ShellCommand shellCommand
      , cwd = Nothing
      , env = Nothing
      , std_in = Inherit
      , std_out = CreatePipe
      , std_err = Inherit
      , close_fds = True
#if __GLASGOW_HASKELL__ >= 702
      , create_group = True
#endif
      }
