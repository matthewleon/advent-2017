module Lib
    ( someFunc
    ) where

import Control.Arrow ((>>>))
import Data.Bifunctor (bimap)
import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import qualified Data.List as L
import qualified Data.IntMap as IM
import qualified Data.Tree as T

type Graph = IM.IntMap [Int]

someFunc :: IO ()
someFunc = do
  input <- readFile "input"
  let graph = parseGraph input
  print graph
  let tree = allPaths 0 graph
  --putStrLn $ T.drawTree $ show <$> tree
  print $ maxVal tree
  print $ maxVal2 tree

parseGraph :: String -> Graph
parseGraph =
  lines
  >>> map (bimap read (read . tail) . break (== '/'))
  >>> foldl'
        (\m (i, j) -> IM.insertWith (++) j [i] $ IM.insertWith (++) i [j] m)
        IM.empty

allPaths :: Int -> Graph -> T.Tree Int
allPaths i g = T.Node i $ map subtree (fromJust $ IM.lookup i g)
  where subtree i' = allPaths i' $ removePath i i' g

removePath :: Int -> Int -> Graph -> Graph
removePath i j = IM.adjust (L.delete i) j . IM.adjust (L.delete j) i

maxVal :: T.Tree Int -> Int
maxVal (T.Node i []) = i
maxVal (T.Node i xs) = 2 * i + maximum (maxVal <$> xs)

maxVal2 :: T.Tree Int -> (Int, Int)
maxVal2 (T.Node i []) = (1, i)
maxVal2 (T.Node i xs) = bimap (+1) (+ (i*2)) $ maximum (maxVal2 <$> xs)
