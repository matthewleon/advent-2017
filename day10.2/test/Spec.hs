import Lib

main :: IO ()
main = do
  print $ runTie ""
  print $ runTie "AoC 2017"
  print "33efeb34ea91902bb2f59c9920caa6cd"
  print $ runTie "AoC 2017" == "33efeb34ea91902bb2f59c9920caa6cd"
  print $ runTie "1,2,3"
  print "3efbe78a8d82f29979031a4aa0b16a9d"
  print $ runTie "1,2,3" == "3efbe78a8d82f29979031a4aa0b16a9d"
  print $ runTie "1,2,4"
  print "63960835bcdc130f0b66d7ff4f6a5a8e"
  print $ runTie "1,2,4" == "63960835bcdc130f0b66d7ff4f6a5a8e"
