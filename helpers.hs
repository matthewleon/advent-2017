-- some helper functions to copy and paste in

import Data.Char (isAlphaNum, isDigit)

readInput :: IO String
readInput = readFile "input"

readInputLines :: IO [String]
readInputLines = lines <$> readInput

alphaNumStrings :: String -> [String]
alphaNumStrings [] = []
alphaNumStrings s =
  let (junk, startAlphaNum) = break isAlphaNum s
      (alphaNum, rest)      = span isAlphaNum startAlphaNum
  in  alphaNum : alphaNumStrings rest

ints :: String -> [Int]
ints [] = []
ints s =
  let (junk, startNum) = break isDigit s
      (num, rest)      = span isDigit startNum
  in  read num : ints rest
