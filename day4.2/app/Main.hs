module Main where

import Lib

main :: IO ()
main = putStrLn . show . length . filter valid . lines =<< readFile "input"
