module Lib
    ( jumps
    ) where

import Debug.Trace (traceShow)
import Data.Vector (Vector, fromList, (!?), modify)
import qualified Data.Vector.Mutable as M

jumps :: String -> Int
jumps = go 0 0 . fromList . fmap read . words
  where
  go :: Int -> Int -> Vector Int -> Int
  go offset counter offsets = case offsets !? offset of
    Just offset' -> go (offset + offset') (counter + 1) 
                    $ modify (\v -> M.modify v (+1) offset) offsets
    Nothing -> counter
