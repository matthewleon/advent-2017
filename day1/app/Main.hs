module Main where

import Prelude

import Data.Char (digitToInt)
import Lib (circledMatchSum)

main :: IO ()
main = 
  readFile "input" >>=
    putStrLn . show . circledMatchSum . fmap digitToInt . init
