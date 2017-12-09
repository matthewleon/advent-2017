module Main where

import Lib

main :: IO ()
main = do
  input <- readFile "input"
  print . scoreTree . getTree $ parseInput input
