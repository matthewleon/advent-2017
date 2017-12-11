module Lib where

import Data.List.Split (splitOn)
import Data.Foldable (foldl')

move :: String -> (Int, Int)
move = move' . splitOn ","
  where
  move' :: [String] -> (Int, Int)
  move' = foldl' go (0, 0)
    where
    go :: (Int, Int) -> String -> (Int, Int)
    go (x, y) = goDir
      where
      goDir "n" = (x, y+2)
      goDir "ne" = (x+1, y+1)
      goDir "e" = (x+2, y)
      goDir "se" = (x+1, y-1)
      goDir "s" = (x, y-2)
      goDir "sw" = (x-1, y-1)
      goDir "w" = (x-2, y)
      goDir "nw" = (x-1, y+1)

stepsAway :: (Int, Int) -> Int
stepsAway (x, y) =
  ((abs x) `div` 2) + ((abs y) `div` 2) + (if odd x || odd y then 1 else 0)

-- part 2
furthest :: String -> Int
furthest = snd . move' . splitOn ","
  where
  move' :: [String] -> ((Int, Int), Int)
  move' = foldl' go ((0, 0), 0)
    where
    go :: ((Int, Int), Int) -> String -> ((Int, Int), Int)
    go ((x, y), maxDistance) dir =
      if stepsAway (goDir dir) > maxDistance
        then ((goDir dir), stepsAway $ goDir dir)
        else ((goDir dir), maxDistance)
      where
      goDir :: String -> (Int, Int)
      goDir "n" = (x, y+2)
      goDir "ne" = (x+1, y+1)
      goDir "e" = (x+2, y)
      goDir "se" = (x+1, y-1)
      goDir "s" = (x, y-2)
      goDir "sw" = (x-1, y-1)
      goDir "w" = (x-2, y)
      goDir "nw" = (x-1, y+1)
