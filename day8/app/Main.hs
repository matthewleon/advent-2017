module Main where

import Lib

import Data.Foldable (traverse_)

main :: IO ()
main = do
  input <- readFile "input"
  traverse_ (print . parseInstruction) $ lines input
  let instructions = parseInstruction <$> lines input
  print $ run instructions
  print $ largestVal (run instructions)
