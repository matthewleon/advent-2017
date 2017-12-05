module Lib
    ( jumps
    ) where

import Debug.Trace (traceShow)
import Data.Vector (Vector, fromList, (!?), modify)
import qualified Data.Vector.Mutable as M

jumps :: String -> (Int, Vector Int)
jumps = go 0 0 . fromList . fmap read . words
  where
  go :: Int -> Int -> Vector Int -> (Int, Vector Int)
  go offset counter offsets = case offsets !? offset of
    Just offset' -> go (offset + offset') (counter + 1) 
                    $ modify
                        (\v -> M.modify v
                               (if offset' >= 3 then (\x -> x - 1) else (+1))
                               offset
                        ) offsets
    Nothing -> (counter, offsets)
