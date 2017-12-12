module Lib where

import Data.Char (isDigit)
import qualified Data.Graph as G
import Data.List.Split (splitOn)

data InputNode = InputNode Int [Int]
  deriving (Show)

parseInput :: String -> [InputNode]
parseInput = fmap parseInputNode . lines

parseInputNode :: String -> InputNode
parseInputNode s =
  let (nodeID, rest) = span isDigit s
      nodeIDs        = splitOn "," $ drop 5 rest
  in InputNode (read nodeID) $ read <$> nodeIDs

mkGraph :: [InputNode] -> G.Graph
mkGraph =
  fst' . G.graphFromEdges . fmap (\(InputNode nid nids) -> (nid, nid, nids))
  where fst' (g, _, _) = g

nodesInZeroGroup :: String -> [Int]
nodesInZeroGroup = flip G.reachable 0 . mkGraph . parseInput

numGroups :: String -> Int
numGroups = length . G.scc . mkGraph . parseInput
