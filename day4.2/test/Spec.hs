import Lib

main :: IO ()
main = do
  putStrLn . show $ valid "abcde fghij"
  putStrLn . show . not $ valid "abcde xyz ecdab"
  putStrLn . show $ valid "a ab abc abd abf abj"
  putStrLn . show $ valid "iiii oiii ooii oooi oooo"
  putStrLn . show . not $ valid "oiii ioii iioi iiio"
