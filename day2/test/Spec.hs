import Lib (checkSum)

input :: String
input = "5 1 9 5\n7 5 3\n 2 4 6 8"

main :: IO ()
main = putStrLn . show $ checkSum ((fmap read . words) <$> lines input) == 18
