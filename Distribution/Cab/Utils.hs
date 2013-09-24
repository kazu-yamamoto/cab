module Distribution.Cab.Utils where

import Data.List

-- |
-- >>> fromDotted "1.2.3"
-- [1,2,3]
fromDotted :: String -> [Int]
fromDotted [] = []
fromDotted xs = case break (=='.') xs of
    (x,"") -> [read x :: Int]
    (x,_:ys) -> (read x :: Int) : fromDotted ys

-- |
-- >>> toDotted [1,2,3]
-- "1.2.3"
toDotted :: [Int] -> String
toDotted = intercalate "." . map show

-- |
-- >>> split 4 "0123457689"
-- ["0123","4576","89"]
split :: Int -> [a] -> [[a]]
split _ [] = []
split n ss = x : split n rest
  where
    (x,rest) = splitAt n ss
