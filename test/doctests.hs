module Main where

import Test.DocTest

main :: IO ()
main = doctest [
    "Distribution/Cab/Utils"
  ]
