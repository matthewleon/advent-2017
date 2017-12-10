module Lib where

import Data.Foldable (foldl')
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as V

tie :: Int -> [Int] -> V.Vector Word8
tie sz = (\(v, _, _) -> v) . foldl' go (V.enumFromN 0 (fromIntegral sz), 0, 0)

go :: (V.Vector Word8, Int, Int) -> Int -> (V.Vector Word8, Int, Int)
go (vec, pos, skip) len =
  let sz = V.length vec
      doubleVec = V.concat [vec, vec]
      slice     = V.unsafeSlice pos len doubleVec
      reverseSlice = V.reverse slice
      overlap = max ((pos + len) - sz) 0
      vec' =
           (V.unsafeDrop (len - overlap) reverseSlice)
        V.++ (V.unsafeDrop overlap (V.unsafeTake pos vec))
        V.++ (V.unsafeTake (len - overlap) reverseSlice)
        V.++ (V.unsafeDrop (min (pos + len) sz) vec)
      pos' = (pos + len + skip) `mod` sz
      skip' = skip + 1
  in (vec', pos', skip')
