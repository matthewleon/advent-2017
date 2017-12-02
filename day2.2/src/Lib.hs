module Lib
    ( checkSum
    ) where

checkSum :: [[Int]] -> Int
checkSum rows = sum $ evenDivision <$> rows
  where
  evenDivision :: [Int] -> Int
  evenDivision [] = error "the world is on fire"
  evenDivision (x:xs) =
    case findDivider xs of
      Just y -> y
      _      -> evenDivision xs
    where 
    findDivider [] = Nothing
    findDivider (x':xs')
      | x > x' && x `mod` x' == 0 = Just $ x `div` x'
      | x' `mod` x == 0 = Just $x' `div` x
      | otherwise = findDivider xs'
