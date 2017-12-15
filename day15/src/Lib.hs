module Lib
    ( someFunc
    ) where

input = (289, 629)
factors = (16807, 48271)
divisor = 2147483647

iter :: Int -> Int -> Int
iter prevVal factor = (prevVal * factor) `mod` divisor

vals :: Int -> Int -> Int -> [Int]
vals startVal factor multVal = drop 1 $ go startVal
  where go x =
          if x `mod` multVal == 0
              then x : go (iter x factor)
              else go (iter x factor)

countMatches :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> Int
countMatches (inputA, inputB) (factorA, factorB) (multA, multB) numPairs =
  length . filter id 
    $ zipWith matchlow16 (vals inputA factorA multA) (take numPairs (vals inputB factorB multB))
  where
  matchlow16 :: Int -> Int -> Bool
  matchlow16 x y = (x `mod` (2 ^ 16)) == (y `mod` (2 ^ 16))

multVals = (4, 8)

someFunc :: IO ()
someFunc = do
  print $ take 5 $ vals 65 (fst factors) 1
  print $ take 5 $ vals 8921 (snd factors) 1
  print $ countMatches (65, 8921) factors (1, 1) 4
  print $ countMatches input factors (1, 1) 40000000
  print $ countMatches input factors multVals 5000000
