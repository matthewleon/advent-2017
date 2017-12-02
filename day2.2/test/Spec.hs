import Lib (checkSum)

input :: String
input = "5 9 2 8\n9 4 7 3\n3 8 6 5"

main :: IO ()
main = putStrLn . show $ checkSum ((fmap read . words) <$> lines input) == 9
