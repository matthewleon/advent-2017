module Main where

import Prelude

import Data.Char (digitToInt)
import Data.Vector (fromList)
import Lib (matchSum)

main :: IO ()
main = do
  putStrLn . show $ matchSum (fromList (digitToInt <$> "1212")) == 6
  putStrLn . show $ matchSum (fromList (digitToInt <$> "1221")) == 0
  putStrLn . show $ matchSum (fromList (digitToInt <$> "123425")) == 4
  putStrLn . show $ matchSum (fromList (digitToInt <$> "123123")) == 12
  putStrLn . show $ matchSum (fromList (digitToInt <$> "12131415")) == 4
