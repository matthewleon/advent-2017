module Main where

import Lib

main :: IO ()
main = do
  input <- readFile "input"
  print . _junkCount $ parseInput input
