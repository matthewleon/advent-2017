import Lib
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = putStrLn . show $ cyclesToLoop "0 2 7 0" == 4
