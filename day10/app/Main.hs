module Main where

import Lib
import qualified Data.Vector.Unboxed as V

main :: IO ()
main =  do
  let input = [212,254,178,237,2,0,1,54,167,92,117,125,255,61,159,164]
      result = tie 256 input
  print result
  case V.toList result of
    (x1:x2:_) -> print $ fromIntegral x1 * (fromIntegral x2 :: Int)
