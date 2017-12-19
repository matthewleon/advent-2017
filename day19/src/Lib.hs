{-# LANGUAGE TupleSections #-}

module Lib
    ( someFunc
    ) where

import Data.Char (isUpper)
import Data.Maybe (fromJust, isJust)
import qualified Data.Matrix as M
import qualified Data.Vector as V

import Debug.Trace

type Diagram = M.Matrix Square

parseDiagram :: String -> Diagram
parseDiagram s = 
  let squares = fmap fromChar <$> lines s
      width = length (head squares)
      height = length squares
  in  M.matrix height width $ \(i, j) -> (squares !! (i - 1)) !! (j - 1)

data Square = Empty | Vert | Horiz | Turn | Letter Char
  deriving (Eq)
instance Show Square where show c = [toChar c]

toChar :: Square -> Char
toChar Empty = ' '
toChar Vert  = '|'
toChar Horiz = '-'
toChar Turn = '+'
toChar (Letter c) = c

fromChar :: Char -> Square
fromChar ' ' = Empty
fromChar '|' = Vert
fromChar '-' = Horiz
fromChar '+' = Turn
fromChar c   = Letter c

data Direction = Up | Down | DLeft | DRight
  deriving (Show, Eq)

allDirections :: [Direction]
allDirections = [Up, Down, DLeft, DRight]

data TraversalState = TraversalState {
    inDirection   :: !Direction
  , seenLetters   :: !String
  , coord         :: !(Int, Int)
} deriving (Show)

initState :: Diagram -> TraversalState
initState d = TraversalState Down []
  $ (1,) . (+1) . fromJust $ V.elemIndex Vert (M.getRow 1 d)

transformCoord :: (Int, Int) -> Direction -> (Int, Int)
transformCoord (i, j) coord = case coord of
  Up     -> (i - 1, j)
  Down   -> (i + 1, j)
  DLeft  -> (i, j - 1)
  DRight -> (i, j + 1)

iterState :: Diagram -> TraversalState -> Either String TraversalState
iterState diagram (TraversalState inDirection seenLetters coord) =
  case uncurry M.getElem coord diagram of
    Empty -> Left $ reverse seenLetters
    Letter c -> Right $ TraversalState inDirection (c:seenLetters)
      $ transformCoord coord inDirection
    Turn ->
      let inDirection'
            | inDirection == Up || inDirection == Down =
                if isJust (uncurry M.safeGet (transformCoord coord DRight) diagram)
                   && uncurry M.getElem (transformCoord coord DLeft) diagram /= Horiz
                then DRight
                else DLeft
            | isJust (uncurry M.safeGet (transformCoord coord Down) diagram)
              && uncurry M.getElem (transformCoord coord Up) diagram /= Vert
              = Down
            | otherwise = Up
      in Right $ TraversalState inDirection' seenLetters
        $ transformCoord coord inDirection'
    _ -> Right $ TraversalState inDirection seenLetters
      $ transformCoord coord inDirection

solveA :: Diagram -> String
solveA d = go $ initState d
  where
  go traversalState = case iterState d traversalState of
    Left s -> s
    Right traversalState' ->
      --traceShow traversalState' $
      go $! traversalState'

solveB :: Diagram -> Int
solveB d = go 0 $ initState d
  where
  go i traversalState = i `seq` case iterState d traversalState of
    Left s -> i
    Right traversalState' ->
      --traceShow traversalState' $
      go (i + 1) $! traversalState'

someFunc :: IO ()
someFunc = do
  diagram <- parseDiagram <$> readFile "input"
  --putStrLn $ M.prettyMatrix diagram
  print $ initState diagram
  print $ solveA diagram
  print $ solveB diagram
