import Lib

main :: IO ()
main = do
  putStrLn . show $ parseDisc "ifqsqyz (223) -> vpfoaw, yrmmga"
    == Disc "ifqsqyz" 223 ["vpfoaw","yrmmga"]
  testinput <- readFile "testinput"
  putStrLn . drawTree . fmap show . tagWithCumWeight . makeWeightTree $ parseDisc <$> lines testinput
  putStrLn . show . correctWeight . tagWithCumWeight . makeWeightTree $ parseDisc <$> lines testinput
