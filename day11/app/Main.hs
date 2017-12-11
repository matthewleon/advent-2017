module Main where

import Lib

main :: IO ()
main = do
  print . stepsAway . move =<< readFile "input"
  print . furthest =<< readFile "input"
