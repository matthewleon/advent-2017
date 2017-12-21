module Lib
    ( someFunc
    ) where

import Data.Foldable (traverse_)
import Data.List (nub)
import Data.List.Extra (splitOn, transpose)
import Data.Maybe (mapMaybe)
import Data.Matrix (Matrix, matrix, prettyMatrix)
import qualified Data.Matrix as M
import Debug.Trace

type Pixel = Bool
type Grid = Matrix Pixel
data Rule = Rule !Grid !Grid
  deriving (Show, Eq)

someFunc :: IO ()
someFunc = do
  expandedRules <- expandRules . parseRules <$> readFile "input"
  print $ countOnPixels $ enhancen 5 expandedRules startGrid
  print $ countOnPixels $ enhancen 18 expandedRules startGrid
  --print $ transforms $ M.fromLists [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
  --traverse_ (putStrLn . ruleStr) expandedRules
  --expandedRulesputStrLn "match"
  --putStrLn $ gridStr $ matchRules startGrid expandedRules
  {-
  putStrLn "enhance"
  putStrLn $ gridStr $ enhanceGrid expandedRules startGrid
  putStrLn "enhancen"
  putStrLn $ gridStr $ enhancen 2 expandedRules startGrid
  -}
  --print $ matchRules startGrid expandedRules
  --putStrLn $ gridStr startGrid
  --putStrLn $ gridStr $ enhanceGrid expandedRules startGrid

  {-
  print $ breakGrid startGrid
  putStrLn $ gridStr $ regrid $ breakGrid startGrid
  putStrLn $ gridStr $ regrid $ breakGrid $ M.joinBlocks (startGrid, startGrid, startGrid, startGrid)
  -}
--someFunc = traverse_ (putStrLn . ruleStr) . parseRules =<< readFile "input"


startGrid :: Grid
startGrid = parseGrid ".#./..#/###"

enhancen :: Int -> [Rule] -> Grid -> Grid
enhancen n rules g = iterate (enhanceGrid rules) g !! n

enhanceGrid :: [Rule] -> Grid -> Grid
enhanceGrid rules = regrid . fmap enhanceSubGrid . breakGrid
  where
  enhanceSubGrid :: Grid -> Grid
  enhanceSubGrid g = matchRules g rules

-- partial
matchRules :: Grid -> [Rule] -> Grid
matchRules g = head . mapMaybe (matchRule g)

matchRule :: Grid -> Rule -> Maybe Grid
matchRule g (Rule l r) = if g == l then Just r else Nothing

breakGrid :: Grid -> Matrix Grid
breakGrid g =
  let stride = if M.nrows g `mod` 2 == 0 then 2 else 3
      dim    = M.nrows g `div` stride
  in matrix dim dim $ \(i, j) ->
        M.submatrix ((i-1) * stride + 1) (i * stride)
                    ((j-1) * stride + 1) (j * stride) g

regrid :: Matrix Grid -> Grid
regrid m =
  let subStride = M.nrows $ M.getElem 1 1 m
      dim = M.nrows m * subStride
  in  matrix dim dim $ \(i, j) ->
        M.getElem (((i - 1) `mod` subStride) + 1) (((j - 1) `mod` subStride) + 1)
        $ M.getElem (((i - 1) `div` subStride) + 1) (((j - 1) `div` subStride) + 1) m

countOnPixels :: Grid -> Int
countOnPixels = length . filter id . M.toList

expandRules :: [Rule] -> [Rule]
expandRules = concatMap expandRule
  where
  expandRule (Rule l r) = (`Rule` r) <$> transforms l

transforms :: Eq a => Matrix a -> [Matrix a]
transforms m = nub [m, rot1 m, rot2 m, rot3 m,
                flipv m, rot1 (flipv m), rot2 (flipv m), rot3 (flipv m),
                fliph m, rot1 (fliph m), rot2 (fliph m), rot3 (fliph m)]
    where
    rot1 = M.fromLists . map reverse . transpose . M.toLists
    rot2 = M.fromLists . map reverse . transpose
                       . map reverse . transpose . M.toLists
    rot3 = M.fromLists . reverse . transpose . M.toLists
    flipv = M.fromLists . reverse . M.toLists
    fliph = M.fromLists . map reverse . M.toLists

pixelChar :: Pixel -> Char
pixelChar True = '#'
pixelChar False = '.'

gridStr :: Grid -> String
gridStr = prettyMatrix . fmap pixelChar

ruleStr :: Rule -> String
ruleStr (Rule g1 g2) = "Rule\n" ++ gridStr g1 ++ "=>\n" ++ gridStr g2

parseRules :: String -> [Rule]
parseRules = fmap parseRule . lines

parseRule :: String -> Rule
parseRule s = case words s of
  [lgrid, _, rgrid] -> Rule (parseGrid lgrid) (parseGrid rgrid)
  _ -> error $ "unable to parse rule from " ++ s

parseGrid :: String -> Grid
parseGrid = M.fromLists . fmap (fmap parseChar) . splitOn "/"
  where
  parseChar :: Char -> Pixel
  parseChar '.' = False
  parseChar '#' = True
