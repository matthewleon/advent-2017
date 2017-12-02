module Lib
    ( checkSum
    ) where

checkSum :: [[Int]] -> Int
checkSum rows = sum $ difference <$> rows
  where
  -- could use a fold to make this faster
  difference :: [Int] -> Int
  difference row = maximum row - minimum row
