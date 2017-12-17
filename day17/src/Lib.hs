module Lib
    ( someFunc
    ) where

import Debug.Trace (traceShow)

spinLock :: Int -> Int -> ([Int], Int)
spinLock totalSteps numSteps = go 1 0 [0]
  where
  go :: Int -> Int -> [Int] -> ([Int], Int)
  go n currentPos lock =
    if n == (totalSteps + 1)
      then (lock, lock !! ((currentPos + 1) `mod` n))
      else go (n+1) newPos lock'
    where
    newPos = ((currentPos + numSteps) `mod` n) + 1
    (start, end) = splitAt newPos lock
    lock'  = start ++ (n : end)

numsAfterZero :: Int -> Int -> [Int]
numsAfterZero maxSteps numSteps = go 1 0 []
  where
  go n currentPos acc =
    if n == maxSteps +1
      then acc
      else
        let newPos = ((currentPos + numSteps) `mod` n) + 1
            acc' = if newPos == 1 then n:acc else acc
        in  go (n+1) newPos acc'

someFunc :: IO ()
someFunc = do
  --print . snd $ spinLock 2017 3
  print . snd $ spinLock 2017 343
  print . head $ numsAfterZero 50000000 343
  --print . head $ numsAfterZero 10000000 343
  --print . head $ numsAfterZero 10000000000 343
  --print . snd $ spinLock 1000000 343

  -- nothing can be inserted before the zero, so the question is when does it stop right after the zero?
  -- so far on 1, 2, 10, 46...
  -- don't actually need to manipulate the list, just do the calculation
