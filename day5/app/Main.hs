module Main where

import Lib

main :: IO ()
main = putStrLn . show =<< jumps <$> readFile "input"
