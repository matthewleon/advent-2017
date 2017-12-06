import Lib
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  putStrLn . show
    $ redistribute (V.fromList [0, 2, 7, 0]) == V.fromList [2, 4, 1, 2]
  putStrLn . show
    $ redistribute (V.fromList [2, 4, 1, 2]) == V.fromList [3, 1, 2, 3]
  putStrLn . show $ cyclesToLoop "0 2 7 0" == 5
