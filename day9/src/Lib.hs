module Lib where

import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import qualified Data.Tree as T
import qualified Data.Tree.Zipper as TZ

data ParseState = ParseState {
    _treePos    :: TZ.TreePos TZ.Empty ()
  , _inJunk     :: Bool
  , _cancelNext :: Bool
}
  deriving (Show)

initState :: ParseState
initState = ParseState {
    _treePos    = TZ.fromForest []
  , _inJunk     = False
  , _cancelNext = False
}

parseInput :: String -> ParseState
parseInput = foldl' parse initState
  where
  parse :: ParseState -> Char -> ParseState
  -- ignoring character
  parse (ParseState tp jnk True) _   = ParseState tp jnk False

  -- in junk
  -- ignore next character
  parse (ParseState tp True _)   '!' = ParseState tp True True
  -- conclude junk
  parse (ParseState tp True _)   '>' = ParseState tp False False
  -- read junk
  parse (ParseState tp True _)   _   = ParseState tp True False

  -- out of junk
  -- enter junk
  parse (ParseState tp False _)   '<' = ParseState tp True False
  -- enter new group
  parse (ParseState tp False _)   '{' = ParseState tp' False False
    where tp' = TZ.children $ TZ.insert (T.Node () []) tp
  -- exit group
  parse (ParseState tp False _)   '}' = ParseState tp' False False
    where tp' = TZ.nextSpace (fromJust $ TZ.parent tp)
  -- commas do nothing
  parse (ParseState tp False _)   ',' = ParseState tp False False
  -- should never happen
  parse (ParseState tp False _)    c  = error $ "invalid input: " ++ show c

getTree :: ParseState -> T.Tree ()
-- there should always be a "previous tree" at the end
getTree = TZ.toTree . fromJust . TZ.prevTree . _treePos

scoreTree :: T.Tree a -> Int
scoreTree = go 1
  where
  go :: Int -> T.Tree a -> Int
  go i (T.Node _ children) = i + sum (go (i+1) <$> children)

showTree :: ParseState -> String
showTree = T.drawTree . fmap (const "()") . getTree
