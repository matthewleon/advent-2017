module Main where

import Prelude

import Data.Char (digitToInt)
import Lib (circledMatchSum)

main :: IO ()
main = do
  putStrLn . show $ circledMatchSum (digitToInt <$> "1122") == 3
  putStrLn . show $ circledMatchSum (digitToInt <$> "1111") == 4
  putStrLn . show $ circledMatchSum (digitToInt <$> "1234") == 0
  putStrLn . show $ circledMatchSum (digitToInt <$> "91212129") == 9
