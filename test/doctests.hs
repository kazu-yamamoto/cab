module Main where

import Test.DocTest

main :: IO ()
main = doctest [
    "-optP-include", "-optPdist/build/autogen/cabal_macros.h",

    "Distribution/Cab"
  ]
