module Main where

import Lib (firstLargerValue)

input :: Int
input = 277678

main :: IO ()
main = putStrLn . show $ firstLargerValue input
