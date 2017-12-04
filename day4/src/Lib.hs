module Lib
    ( valid
    ) where

import Data.List (nub)

valid :: String -> Bool
valid = validate . words
  -- we could use ordNub and be faster
  where validate xs = nub xs == xs
