{-# LANGUAGE CPP #-}

module Env (unsetEnv) where

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
unsetEnv :: String -> IO ()
unsetEnv _ = return ()
#else
import System.Posix.Env (unsetEnv)
#endif
