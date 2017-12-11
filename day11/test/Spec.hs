import Lib

main :: IO ()
main = do
  print $ move "ne,ne,ne"
  print $ move "ne,ne,sw,sw"
  print $ move "ne,ne,s,s"
  print $ move "se,sw,se,sw,sw"

  print . stepsAway $ move "ne,ne,ne"
  print . stepsAway $ move "ne,ne,sw,sw"
  print . stepsAway $ move "ne,ne,s,s"
  print . stepsAway $ move "se,sw,se,sw,sw"
