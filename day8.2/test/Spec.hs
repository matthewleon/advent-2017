import Lib

import Data.Foldable (traverse_)

main :: IO ()
main = do
  -- manually check for sanity
  print $ parseInstruction "a inc 5 if a > 1"
  -- again
  testinput <- readFile "testinput"
  let instructions = parseInstruction <$> lines testinput
  traverse_ print instructions
  -- check for sanity
  print $ initRegisters instructions
  print $ run instructions
  print $ fst (run instructions) == 10
