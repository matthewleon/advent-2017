module Lib
    ( someFunc
    ) where

import Control.Monad (when, void)
import Control.Monad.ST (ST, runST)
import Data.Bits (xor)
import Data.List (foldl1')
import Data.Foldable (foldl')
import Data.Word (Word8)
import Data.List.Split (chunksOf)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Char as C
import Text.Printf (printf)

input :: String
input = "jzgqcdpd"

gridSize :: Int
gridSize = 128

someFunc :: IO ()
someFunc = do
  putStrLn $ knotHash "flqrgnkx-0"
  putStrLn $ knotHash "flqrgnkx-1"
  putStrLn $ hexToBin "a0c2017"
  let inputs = [input ++ "-" ++ show i | i <- [0..127]]
  print inputs
  putStrLn "part 1 solution"
  print . sum $ map usedCells inputs
  --putStrLn $ binMap inputs
  --print $ binMapVec inputs
  print . countRegions $ binMapVec inputs

countRegions :: V.Vector (V.Vector Bool) -> Int
countRegions v = runST $ do
  mutV <- V.thaw =<< traverse V.thaw v
  go 0 (0, 0) mutV

  where
  go nRegions (x, y) mutV = do
    val <- flip MV.read x =<< MV.read mutV y
    filled <- fill (x, y)
    if filled then next (nRegions + 1) (x, y) else next nRegions (x, y)

    where
    fill (x, y) = do
      val <- flip MV.read x =<< MV.read mutV y
      when val $ do
        (\v' -> MV.write v' x False) =<< MV.read mutV y
        when (x > 0) (void $ fill (x - 1, y))
        when (x < 127) (void $ fill (x + 1, y))
        when (y > 0) (void $ fill (x, y - 1))
        when (y < 127) (void $ fill (x, y + 1))
      return val

    next nRegions' (127, 127) = return nRegions'
    next nRegions' (127, y)   = go nRegions' (0, y + 1) mutV
    next nRegions' (x, y)   = go nRegions' (x + 1, y) mutV

binMapVec :: [String] -> V.Vector (V.Vector Bool)
binMapVec = V.fromList . fmap (V.fromList . fmap (== '1') . hexToBin . knotHash)

binMap :: [String] -> String
binMap = unlines . fmap (hexToBin . knotHash)

usedCells :: String -> Int
usedCells = length . filter (== '1') . hexToBin . knotHash

hexToBin :: String -> String
hexToBin = concatMap (printf "%04b") . fmap C.digitToInt

-- crap from day10 below

knotHash :: String -> String
knotHash s =
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
