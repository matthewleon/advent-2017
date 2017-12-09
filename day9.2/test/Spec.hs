import Lib

main :: IO ()
main = do
  putStrLn "check these manually"
  putStrLn . showTree $ parseInput "{}"
  putStrLn . showTree $ parseInput "{{}{}}"
  putStrLn . showTree $ parseInput "{{<a>},{<a>},{<a>},{<a>}}"
  print . scoreTree . getTree $ parseInput "{{<a>},{<a>},{<a>},{<a>}}"
  print . scoreTree . getTree $ parseInput "{{<!!>},{<!!>},{<!!>},{<!!>}}"
  print . scoreTree . getTree $ parseInput "{{<a!>},{<a!>},{<a!>},{<ab>}}"
