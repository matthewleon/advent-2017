{-# LANGUAGE TupleSections #-}

module Lib where

import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V

cyclesToLoop :: String -> Int
cyclesToLoop = cyclesToLoop' . fmap read . words

cyclesToLoop' :: [Int] -> Int
cyclesToLoop' = go 0 S.empty . V.fromList
  where
  go :: Int -> S.Set (V.Vector Int) -> V.Vector Int -> Int
  go steps history state =
    if state `S.member` history
      then steps
      else go (steps + 1) (state `S.insert` history) (redistribute state)

redistribute :: V.Vector Int -> V.Vector Int
redistribute state =
  let i = V.maxIndex state
      v = V.unsafeIndex state i
      l = V.length state
  in V.unsafeAccum (\a _ -> a + 1)
                   (V.unsafeUpd state [(i, 0)])
                   ((,()) <$> (`mod` l) <$> [(i+1)..(i+v)])
