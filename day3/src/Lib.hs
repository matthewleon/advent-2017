module Lib
    ( distance
    ) where

import Prelude
import Data.List (findIndex)
import Data.Maybe (fromJust)

distance :: Int -> Int
distance x =
  let odds = filter odd [1..]
      squaredOdds = zipWith (*) odds odds
      squareDistanceFromCenter = fromJust $ findIndex (>= x) squaredOdds
      squareSideLength = odds !! squareDistanceFromCenter
      squareHighestIndex = squareSideLength * squareSideLength
      distanceFromCenterOfSquareSide =
        abs
          $ ((squareHighestIndex - x) `mod` (squareSideLength - 1))
            - ((squareSideLength - 1) `div` 2)
  in squareDistanceFromCenter + distanceFromCenterOfSquareSide
