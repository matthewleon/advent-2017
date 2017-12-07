module Main where

import Lib

main :: IO ()
main = putStrLn . show . bottom =<< readFile "input"
