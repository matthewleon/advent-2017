module Main where

import Lib
import qualified Data.Vector.Unboxed as V

main :: IO ()
main =  do
  let input = "212,254,178,237,2,0,1,54,167,92,117,125,255,61,159,164"
  print $ runTie input
