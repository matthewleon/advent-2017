module Main where

import Lib

main :: IO ()
main = putStrLn . show . fst =<< jumps <$> readFile "input"
