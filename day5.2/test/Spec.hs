import Lib (jumps)
import Data.Vector (fromList)

input :: String
input = "0 3 0 1 -3"

main :: IO ()
main = putStrLn . show $ jumps input == (10, fromList [2, 3, 2, 3,-1])
