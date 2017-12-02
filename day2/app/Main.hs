module Main where

import Lib (checkSum)

main :: IO ()
main = do
  input <- readFile "input"
  putStrLn . show $ checkSum ((fmap read . words) <$> lines input)
