import Lib

main :: IO ()
main = do
  putStrLn . show $ firstLargerValue 0 == 1
  putStrLn . show $ firstLargerValue 1 == 2
  putStrLn . show $ firstLargerValue 55 == 57
  putStrLn . show $ firstLargerValue 747 == 806

-- used these to debug nextCoord
{-
  putStrLn . show $ nextCoord (1, -1)
  putStrLn . show $ nextCoord (1, 0)
  putStrLn . show $ nextCoord (1, 1)
  putStrLn . show $ nextCoord (0, 1)
  putStrLn . show $ nextCoord (-1, 1)
  putStrLn . show $ nextCoord (-1, -1)
  putStrLn . show $ nextCoord (0, -1)
  putStrLn . show $ nextCoord (1, -1)
-}
