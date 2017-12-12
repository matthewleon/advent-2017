module Main where

import Lib

main :: IO ()
main = do
  print . length . nodesInZeroGroup =<< readFile "input"
  print . numGroups =<< readFile "input"
