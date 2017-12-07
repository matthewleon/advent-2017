import Lib

main :: IO ()
main = do
  putStrLn . show $ parseDisc "ifqsqyz (223) -> vpfoaw, yrmmga"
    == Disc "ifqsqyz" 223 ["vpfoaw","yrmmga"]
  testinput <- readFile "testinput"
  putStrLn . show $ bottom testinput == "tknk"
