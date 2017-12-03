module Lib where

import Prelude
import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Coord = (Int, Int)

type Grid = Map Coord Int

origin :: Coord
origin = (0, 0)

initGrid :: Grid
initGrid = M.singleton origin 1

-- | From our current cell, which cell is the next to fill in
-- this logic could probably be simpler, but whatever
nextCoord :: Coord -> Coord
nextCoord (x, y)
  -- corner cells
  | abs x == abs y =
    if (y <= 0)
      then (x + 1, y) -- lower left and right, go right
      else
        if x >= 0
          then (x - 1, y) -- upper right, go left
          else (x, y - 1) -- upper left, go down
  -- wall cells
  | x > 0 && x > abs y    = (x, y + 1) -- right wall:    go up
  | x < 0 && (-x) > abs y = (x, y - 1) -- left wall:     go down
  | y > 0 && y > x        = (x - 1, y) -- top wall:      go left
  | otherwise             = (x + 1, y) -- bottom wall:   go right

-- | What value should we fill in the current coordinate with
coordValue :: Grid -> Coord -> Int
coordValue g (x, y) = sum $ mapMaybe (flip M.lookup g) neighborCoords
  where
  neighborCoords = [
      (x + 1, y - 1) -- bottom right
    , (x + 1, y)     -- right
    , (x + 1, y + 1) -- top right
    , (x,     y + 1) -- top
    , (x - 1, y + 1) -- top left
    , (x - 1, y)     -- left
    , (x - 1, y - 1) -- bottom left
    , (x    , y - 1) -- bottom
    ]

firstLargerValue :: Int -> Int
firstLargerValue v = go initGrid $ nextCoord origin
  where
  go :: Grid -> Coord -> Int
  go g c =
    let val = coordValue g c
    in  if val > v
        then val
        else go (M.insert c val g) $ nextCoord c
