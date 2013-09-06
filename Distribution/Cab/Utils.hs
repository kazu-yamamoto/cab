module Distribution.Cab.Utils where

import Data.List

fromDotted :: String -> [Int]
fromDotted [] = []
fromDotted xs = case break (=='.') xs of
    (x,"") -> [read x :: Int]
    (x,_:ys) -> (read x :: Int) : fromDotted ys

toDotted :: [Int] -> String
toDotted = joinBy "." . map show

joinBy :: String -> [String] -> String
joinBy = intercalate

split :: Int -> [a] -> [[a]]
split _ [] = []
split n ss = x : split n rest
  where
    (x,rest) = splitAt n ss
