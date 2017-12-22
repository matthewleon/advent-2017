{-# LANGUAGE RecordWildCards, RankNTypes #-}

module Lib
    ( someFunc
    ) where

import Control.Monad.ST (ST, runST)
import Data.List.Extra (chunksOf)
import qualified Data.Map.Strict as MS
import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import Debug.Trace

type Grid = M.Matrix Infected
type Infected = Bool
data Direction = Up | DRight | Down | DLeft
  deriving (Eq, Show)

-- VGrid side dimension, vector
data VGrid h = VGrid !Int !(MV.MVector h Bool)

data State h = State {
    grid     :: !(VGrid h)
  , position :: !(Int, Int)
  , direction :: !Direction
  , causedInfections :: !Int
}

-- part 2
data State2 = State2 {
    uncleanNodes :: MS.Map (Int, Int) Unclean
  , position2 :: !(Int, Int)
  , direction2 :: !Direction
  , causedInfections2 :: !Int
}
  deriving (Show)

data NodeState = Clear | Unclean !Unclean
  deriving (Show)
data Unclean = Weakened | Infected | Flagged
  deriving (Show)

someFunc :: IO ()
someFunc = do
  input <- readFile "input"
  let numMoves = 10000
      g = parseInput input
      i = runST $ causedInfections <$> makeMoves g numMoves
  print i
  print . causedInfections2 $ itern 10000000 iter2 $ initState2 g
  {-
  let vs = runST $ do
            s <- initState g numMoves
            vgridStr $ grid s
  -}
  --print $ causedInfections $ makeMoves (parseInput input) numMoves
  --

itern :: Int -> (a -> a) -> a -> a
itern n f s = iterate f s !! n

makeMoves :: Grid -> Int -> ST h (State h)
makeMoves g i = do
  is <- initState g i
  go i is
  where
  go 0 s = pure s
  go n s = do
    s' <- iter s
    go (n-1) s'

initState :: Grid -> Int -> ST h (State h)
initState g numMoves = do
  let dim = numMoves `div` 10 - 1
  gridVect <- UV.thaw . V.convert . M.getMatrixAsVector
            $ expandGrid dim dim g
  pure State {
      grid = VGrid dim gridVect
    , position = middlePos dim
    , direction = Up
    , causedInfections = 0
  }

initState2 :: Grid -> State2
initState2 g =
  let dim = M.nrows g
      infectedIndices = V.findIndices id $ M.getMatrixAsVector g
      infectedIndices2d = (\i -> (i `mod` dim, i `div` dim)) <$> infectedIndices
  in State2 {
         uncleanNodes = MS.fromList
                      $ zip (V.toList infectedIndices2d) $ repeat Infected
       , position2 = let (x, y) = middlePos dim in (x-1, y-1)
       , direction2 = Up
       , causedInfections2 = 0
     }

vgridStr :: VGrid h -> ST h String
vgridStr (VGrid dim mv) = do
  v <- UV.freeze mv
  let l = UV.toList v
      cl = fmap (\b -> if b then '#' else '.') l
  pure . unlines $ chunksOf dim cl

iter :: State h -> ST h (State h)
iter State{..} = do
  let (i, j) = position
  infected <- grid `getElem` position
  setElem grid position (not infected)
  let causedInfections' = causedInfections + if infected then 0 else 1
      direction' = (if infected then turnRight else turnLeft) direction
      position' = case direction' of
        Up -> (i-1, j)
        DRight -> (i, j+1)
        Down -> (i+1, j)
        DLeft -> (i, j-1)
  pure $ State grid position' direction' causedInfections'

iter2 :: State2 -> State2
iter2 State2{..} =
  let (x, y) = position2
      nodeState = maybe Clear Unclean $ MS.lookup position2 uncleanNodes
      (nodeState', direction2', causedInfections2') = case nodeState of
        Clear -> (Unclean Weakened, turnLeft direction2, causedInfections2)
        Unclean Weakened -> (Unclean Infected, direction2, causedInfections2 + 1)
        Unclean Infected -> (Unclean Flagged, turnRight direction2, causedInfections2)
        Unclean Flagged -> (Clear, invert direction2, causedInfections2)
      uncleanNodes' = case nodeState' of
        Clear -> MS.delete position2 uncleanNodes
        Unclean s -> MS.insert position2 s uncleanNodes
      position2' = case direction2' of
        Up -> (x, y-1)
        DRight -> (x+1, y)
        Down -> (x, y+1)
        DLeft -> (x-1, y)
  in State2 uncleanNodes' position2' direction2' causedInfections2'

getElem :: VGrid h -> (Int, Int) -> ST h Bool
getElem (VGrid dim v) (i, j) = MV.read v $ (i - 1) * dim + j-1

setElem :: VGrid h -> (Int, Int) -> Bool -> ST h ()
setElem (VGrid dim v) (i, j) = MV.write v ((i - 1) * dim + j-1)

middlePos :: Int -> (Int, Int)
middlePos i = let m = (i `div` 2) + 1 in (m, m)

turnRight :: Direction -> Direction
turnRight Up = DRight
turnRight DRight = Down
turnRight Down = DLeft
turnRight DLeft = Up

turnLeft :: Direction -> Direction
turnLeft Up = DLeft
turnLeft DRight = Up
turnLeft Down = DRight
turnLeft DLeft = Down

invert :: Direction -> Direction
invert Up = Down
invert DRight = DLeft
invert Down = Up
invert DLeft = DRight

prettyGrid :: Grid -> String
prettyGrid = M.prettyMatrix . fmap (\i -> if i then '#' else '.')

expandGrid :: Int -> Int -> Grid -> Grid
expandGrid newRows newCols g =
  let oldRows = M.nrows g
      oldCols = M.ncols g
      newRowsPerMatrix = ((newRows - oldRows) `div` 2)
                       + ((newRows - oldRows) `mod` 2)
      newColsPerMatrix = ((newCols - oldCols) `div` 2)
                       + ((newCols - oldCols) `mod` 2)
      topBottomMatrix = M.fromLists
                      $ replicate newRowsPerMatrix (replicate newCols False)
      leftDRightMatrix = M.fromLists
                      $ replicate oldRows (replicate newColsPerMatrix False)
      in topBottomMatrix
         M.<-> (leftDRightMatrix M.<|> g M.<|> leftDRightMatrix)
         M.<-> topBottomMatrix


parseInput :: String -> Grid
parseInput = fmap (== '#') . M.fromLists . lines
