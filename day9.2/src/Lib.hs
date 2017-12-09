module Lib where

import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import qualified Data.Tree as T
import qualified Data.Tree.Zipper as TZ

data ParseState = ParseState {
    _junkCount  :: Int
  , _inJunk     :: Bool
  , _cancelNext :: Bool
}
  deriving (Show)

initState :: ParseState
initState = ParseState {
    _junkCount  = 0
  , _inJunk     = False , _cancelNext = False
}

parseInput :: String -> ParseState
parseInput = foldl' parse initState
  where
  parse :: ParseState -> Char -> ParseState
  -- ignoring character
  parse (ParseState cnt jnk True) _   = ParseState cnt jnk False

  -- in junk
  -- ignore next character
  parse (ParseState cnt True _)   '!' = ParseState cnt True True
  -- conclude junk
  parse (ParseState cnt True _)   '>' = ParseState cnt False False
  -- read junk
  parse (ParseState cnt True _)   _   = ParseState (cnt + 1) True False

  -- out of junk
  -- enter junk
  parse (ParseState tp False _)   '<' = ParseState tp True False
  parse (ParseState cnt False _)   c  = ParseState cnt False False
