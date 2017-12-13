module Lib
    ( someFunc
    ) where

import Data.Char (isDigit)
import Data.List (elemIndex)

type Depth = Int
type Range = Int
type Delay = Int

someFunc :: IO ()
someFunc = do
  let testInput = [(0, 3), (1, 2), (4, 4), (6, 4)]
  print $ severity 0 testInput
  inputLines <- readInputLines
  let parsedLines = fmap parseLine inputLines
  print $ severity 0 parsedLines
  print $ fmap (`severity'` testInput) [0..100]
  --print $ fmap (`severity` parsedLines) [0..100]
  print . elemIndex 0 $ fmap (`severity'` parsedLines) [0..]

severity :: Delay -> [(Depth, Range)] -> Int
severity delay = sum . fmap depthSeverity
  where
  depthSeverity (depth, range) =
    if (depth + delay) `mod` ((range - 1) * 2) == 0
      then depth * range
      else 0

severity' :: Delay -> [(Depth, Range)] -> Int
severity' delay = sum . fmap depthSeverity
  where
  depthSeverity (depth, range) =
    if (depth + delay) `mod` ((range - 1) * 2) == 0
      then 1
      else 0

parseLine :: String -> (Depth, Range)
parseLine s = case ints s of
  [x, y] -> (x, y)
  _      -> error "parsing"

ints :: String -> [Int]
ints [] = []
ints s =
  let (junk, startNum) = break isDigit s
      (num, rest)      = span isDigit startNum
  in read num : ints rest

readInput :: IO String
readInput = readFile "input"

readInputLines :: IO [String]
readInputLines = lines <$> readInput
