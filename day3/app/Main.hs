module Main where

import Lib (distance)

input :: Int
input = 277678

main :: IO ()
main = putStrLn . show $ distance input
