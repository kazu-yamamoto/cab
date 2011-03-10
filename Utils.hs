module Utils where

import Data.List

fromDotted :: String -> [Int]
fromDotted [] = []
fromDotted xs = case break (=='.') xs of
    (x,"") -> [read x :: Int]
    (x,_:ys) -> (read x :: Int) : fromDotted ys

toDotted :: [Int] -> String
toDotted = concat . intersperse "." . map show
