import Lib (distance)

main :: IO ()
main = do
  -- not edge-casing this
  --putStrLn . show $ distance 1 == 0
  putStrLn . show $ distance 12 == 3
  putStrLn . show $ distance 23 == 2
  putStrLn . show $ distance 1024
  putStrLn . show $ distance 1024 == 31
