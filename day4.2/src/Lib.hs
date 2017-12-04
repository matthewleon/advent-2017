module Lib
    ( valid
    ) where

import Data.List (sort)
import Nub (ordNub)

valid :: String -> Bool
valid = validate . words

validate :: [String] -> Bool
validate = check . fmap sort
  where check xs = ordNub xs == xs
