module Main where

import Lib

main :: IO ()
main = putStrLn . show . cyclesToLoop =<< readFile "input"
