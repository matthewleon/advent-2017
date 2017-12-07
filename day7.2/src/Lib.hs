module Lib where

import Data.Char (isLower, isDigit)
import Data.Ord (comparing)
import qualified Data.Tree  as T
import Data.Foldable (foldl')
import Data.List (find, findIndex, maximumBy, sortOn)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe, fromJust, isJust)
import qualified Data.Map.Strict as M
import Data.List.Split (splitOn)

data Disc = Disc String Int [String]
  deriving (Show, Eq)

getName :: Disc -> String
getName (Disc name _ _ ) = name

parseDisc :: String -> Disc
parseDisc s = 
  let (name, rest)    = span isLower s
      (weight, rest') = span isDigit $ drop 2 rest
      subDiscs        = filter (/= "") . splitOn ", " $ drop 5 rest'
  in Disc name (read weight) subDiscs

makeWeightTree :: [Disc] -> T.Tree (String, Int)
makeWeightTree discs =
  let 
      discSubdiscMap :: Map String (Int, [String])
      discSubdiscMap =
        foldl' (\m (Disc name w subdiscs) -> M.insert name (w, subdiscs) m) 
               M.empty
               discs

      getWeightSubdiscs :: String -> (Int, [String])
      getWeightSubdiscs discName = fromJust $ M.lookup discName discSubdiscMap

      countDescendants :: (Int, [String]) -> Int
      countDescendants (_, subdiscs) =
        length subdiscs + sum (fmap (countDescendants . getWeightSubdiscs) subdiscs)

      discDescendantCountMap :: Map String Int
      discDescendantCountMap = countDescendants <$> discSubdiscMap

      root :: String
      root = fst . last . sortOn snd $ M.toList discDescendantCountMap
  in T.unfoldTree (\name -> let (weight, subdiscs) = getWeightSubdiscs name
                            in ((name, weight), subdiscs)
                  )
                  root

tagWithCumWeight :: T.Tree (String, Int) -> T.Tree (String, Int, Int)
tagWithCumWeight (T.Node (name, weight) []) = T.Node (name, weight, weight) []
tagWithCumWeight (T.Node (name, weight) ns) =
  let children = tagWithCumWeight <$> ns
  in T.Node
       (name, weight, weight + sum ((\(T.Node (_,_,cw) _) -> cw) <$> children))
       children

correctWeight :: T.Tree (String, Int, Int) -> Maybe Int
correctWeight (T.Node _ []) = Nothing
correctWeight (T.Node (_, w, cumW) trees@(t@(T.Node (_,_,tcw) _):ts)) =
  case mapMaybe correctWeight trees of
    i : _ -> Just i
    []    ->
      if all (== tcw) $ map (\(T.Node (_,_,cw) _) -> cw) ts
         then Nothing
         else
           let weightTuples = map (\(T.Node (_,w,cw) _) -> (w, cw)) trees
               weights = map fst weightTuples
               cumWeights = map snd weightTuples
               mostCommonCumWeight = 
                 fst
                 . maximumBy (comparing snd)
                 . M.toList
                 $ foldl' (\m i -> M.insertWith (\_ c -> c + 1) i 1 m) M.empty cumWeights
               outlierIndex = fromJust (findIndex (/= mostCommonCumWeight) cumWeights)
               outlierWeight = weights !! outlierIndex
               outlierCumWeight = cumWeights !! outlierIndex
            in Just $ outlierWeight - (outlierCumWeight - mostCommonCumWeight)

-- export
drawTree = T.drawTree
