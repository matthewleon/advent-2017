module Lib where

import Data.Char (isLower, isDigit)
import Data.Foldable (foldl')
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import Data.List.Split (splitOn)

data Disc = Disc String Int [String]
  deriving (Show, Eq)

bottom :: String -> String
bottom = bottom' . fmap parseDisc . lines

parseDisc :: String -> Disc
parseDisc s = 
  let (name, rest)    = span isLower s
      (weight, rest') = span isDigit $ drop 2 rest
      subDiscs        = filter (/= "") . splitOn ", " $ drop 5 rest'
  in Disc name (read weight) subDiscs

bottom' :: [Disc] -> String
bottom' discs =
  let 
      discSubdiscMap :: Map String [String]
      discSubdiscMap =
        foldl' (\m (Disc name _ subdiscs) -> M.insert name subdiscs m) 
               M.empty
               discs

      getSubdiscs :: String -> [String]
      getSubdiscs discName = fromJust $ M.lookup discName discSubdiscMap

      countDescendents :: [String] -> Int
      countDescendents subdiscs =
        length subdiscs + sum (fmap (countDescendents . getSubdiscs) subdiscs)

      discDescendentCountMap :: Map String Int
      discDescendentCountMap = countDescendents <$> discSubdiscMap

  in  fst . head . reverse . sortOn snd $ M.toList discDescendentCountMap
