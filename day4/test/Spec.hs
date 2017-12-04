import Lib

main :: IO ()
main = do
  putStrLn . show $ valid "aa bb cc dd ee"
  putStrLn . show . not $ valid "aa bb cc dd aa"
  putStrLn . show $ valid "aa bb cc dd aaa"
