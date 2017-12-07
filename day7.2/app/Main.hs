module Main where

import Lib

main :: IO ()
main = do
  input <- readFile "input"
  putStrLn . drawTree . fmap show . tagWithCumWeight . makeWeightTree $ parseDisc <$> lines input
  putStrLn . show . correctWeight . tagWithCumWeight . makeWeightTree $ parseDisc <$> lines input
