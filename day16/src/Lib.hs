module Lib
    ( someFunc
    ) where

import Prelude hiding (splitAt, (++))

import Data.Char (chr, ord)
import Data.IORef
import Control.Monad (foldM, replicateM_)
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (freeze, thaw, (++))
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Vector.Unboxed.Mutable (IOVector, splitAt)

data Move = Spin !Int | Exchange !Int !Int | Partner !Char !Char
  deriving (Show)

parse :: String -> [Move]
parse = fmap parseMove . splitOn ","
  where
  parseMove ('s':s) = Spin (read s)
  parseMove ('x':s) = case splitOn "/" s of
    [s1, s2] -> Exchange (read s1) (read s2)
  parseMove ['p',c1,'/',c2] = Partner c1 c2

applyMove :: IOVector Char -> Move -> IO (IOVector Char)
applyMove v (Spin i) = do
  let len = 16
      (start, end) = splitAt (len - i) v
  iStart <- V.unsafeFreeze start
  iEnd   <- V.unsafeFreeze end
  thaw $ iEnd ++ iStart
applyMove v (Exchange i1 i2) = do
  VM.unsafeSwap v i1 i2
  return v
applyMove v (Partner c1 c2) = do
  iv <- V.unsafeFreeze v
  let i1 = fromJust $ V.elemIndex c1 iv
      i2 = fromJust $ V.elemIndex c2 iv
  VM.unsafeSwap v i1 i2
  return v

applyRotation :: V.Vector Int -> V.Vector Char -> V.Vector Char
applyRotation ps v = V.generate 16 $ V.unsafeIndex v . V.unsafeIndex ps

rotNtimes :: V.Vector Int -> Int -> V.Vector Char -> V.Vector Char
rotNtimes iv = go
  where
  go 0 v = v
  go n v = go (n - 1) (applyRotation iv v)

someFunc :: IO ()
someFunc = do
  parsedInput <- parse . init <$> readFile "input"
  startVec <- V.thaw $ V.fromList ['a'..'p']
  endVec <- freeze =<< foldM applyMove startVec parsedInput
  print endVec
  startVec' <- V.thaw endVec
  endVec' <- freeze =<< foldM applyMove startVec' parsedInput
  print endVec'

  let positionVec = (\c -> ord c - ord 'a') `V.map` endVec
  print positionVec

  startVec' <- V.thaw $ V.fromList ['a'..'p']
  mvRef <- newIORef startVec'
  replicateM_ (1000000000 `mod` 60) $ do
    mv <- readIORef mvRef
    newVec <- foldM applyMove mv parsedInput
    writeIORef mvRef newVec
  print =<< freeze =<< readIORef mvRef

{-
  print $ applyRotation positionVec (V.fromList ['a'..'p'])
  print $ rotNtimes positionVec 1 (V.fromList ['a'..'p'])
  print $ rotNtimes positionVec 2 (V.fromList ['a'..'p'])
  print $ rotNtimes positionVec 1000 (V.fromList ['a'..'p'])
-}
  --print $ rotNtimes positionVec 1000000 (V.fromList ['a'..'p'])
  --print $ rotNtimes positionVec 1000000000 (V.fromList ['a'..'p'])
  --print . last $ iterate 1000000000 (applyRotation positionVec) (V.fromList ['a'..'p'])

  {-
  --replicateM_ 1000000000 $ do
  -}
