module Main where

import Prelude

import Data.Char (digitToInt)
import Data.Vector (fromList)
import Lib (matchSum)

main :: IO ()
main = 
  readFile "input" >>=
    putStrLn . show . matchSum . fromList . fmap digitToInt . init -- clip off "\n"
