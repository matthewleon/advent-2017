import Lib (jumps)

input :: String
input = "0 3 0 1 -3"

main :: IO ()
main = putStrLn . show $ jumps input == 5
