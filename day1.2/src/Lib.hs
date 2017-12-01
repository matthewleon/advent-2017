module Lib
    ( matchSum
    ) where

import Prelude hiding (length)
import Data.Vector (Vector, length, (!), ifoldl')

matchSum :: Vector Int -> Int
matchSum xs = ifoldl'
  (\acc i b ->
    if b == (xs ! ((i + increment) `mod` len))
      then acc + b
      else acc
  ) 0 xs
  where
    len = length xs
    increment = len `div` 2
