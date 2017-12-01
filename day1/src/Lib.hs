module Lib
    ( circledMatchSum
    ) where

import Prelude
import Data.Foldable (foldl')

circledMatchSum :: [Int] -> Int
circledMatchSum = matchSum . circlify

circlify :: [a] -> [a]
circlify xs = if length xs < 2 then xs else last xs : xs

matchSum :: [Int] -> Int
matchSum = snd
  . foldl' (\(prev, acc) a -> (a, if a == prev then acc + prev else acc)) (0, 0)
