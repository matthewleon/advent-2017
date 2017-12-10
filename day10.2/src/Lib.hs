module Lib where

import Data.Bits (xor)
import Data.List (foldl1')
import Data.Foldable (foldl')
import Data.Word (Word8)
import Data.List.Split (chunksOf)
import qualified Data.Vector.Unboxed as V
import qualified Data.Char as C
import Numeric (showHex)
import Text.Printf (printf)

runTie :: String -> String
runTie s =
  let sparseHash = tie $ (C.ord <$> s) ++ [17, 31, 73, 47, 23]
      denseHash :: [Word8]
      denseHash  = fmap (foldl1' xor) . chunksOf 16 $ V.toList sparseHash
  in concatMap (printf "%02x") denseHash

-- modify to run 64 rounds, preserving position and skip size
tie :: [Int] -> V.Vector Word8
tie lens = tie' 64 (V.enumFromN 0 256, 0, 0)
  where
  tie' :: Int -> (V.Vector Word8, Int, Int) -> V.Vector Word8
  tie' 0 (v, _, _) = v
  tie' n state = tie' (n - 1) $ foldl' go state lens

go :: (V.Vector Word8, Int, Int) -> Int -> (V.Vector Word8, Int, Int)
go (vec, pos, skip) len =
  let sz = V.length vec
      doubleVec = V.concat [vec, vec]
      slice     = V.unsafeSlice pos len doubleVec
      reverseSlice = V.reverse slice
      overlap = max ((pos + len) - sz) 0
      vec' = V.concat [
               V.unsafeDrop (len - overlap) reverseSlice
             , V.unsafeDrop overlap (V.unsafeTake pos vec)
             , V.unsafeTake (len - overlap) reverseSlice
             , V.unsafeDrop (min (pos + len) sz) vec
             ]
      pos' = (pos + len + skip) `mod` sz
      skip' = skip + 1
  in (vec', pos', skip')
