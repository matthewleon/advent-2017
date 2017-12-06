{-# LANGUAGE TupleSections #-}

module Lib where

import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V

cyclesToLoop :: String -> Int
cyclesToLoop = cyclesToLoop' . fmap read . words

cyclesToLoop' :: [Int] -> Int
cyclesToLoop' = go 0 M.empty . V.fromList
  where
  go :: Int -> M.Map (V.Vector Int) Int -> V.Vector Int -> Int
  go steps history state =
    case state `M.lookup` history of
      Just step -> steps - step
      Nothing -> go (steps + 1)
                    (M.insert state steps history)
                    (redistribute state)

redistribute :: V.Vector Int -> V.Vector Int
redistribute state =
  let i = V.maxIndex state
      v = V.unsafeIndex state i
      l = V.length state
  in V.unsafeAccum (\a _ -> a + 1)
                   (V.unsafeUpd state [(i, 0)])
                   ((,()) <$> (`mod` l) <$> [(i+1)..(i+v)])
