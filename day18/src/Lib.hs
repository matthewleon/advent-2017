module Lib
    ( someFunc
    ) where

import Data.Char (isAlphaNum, isAlpha, ord)
import Data.Foldable (traverse_)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Debug.Trace

type Program = V.Vector Instruction
type Registers = M.Map Reg Int
type Reg = Char
type Counter = Int
type Frequency = Int

data State = State !Program !Registers !Counter !Frequency
  deriving (Show, Eq)

data State2 = State2 !Program !PrgState !PrgState

data PrgState = Running !Registers !Counter ![Int] !Int
              | Waiting !Int

data RegOrVal = Reg !Reg | Val !Int
  deriving (Show, Eq)
data Instruction =
    Snd !RegOrVal
  | Set !Reg !RegOrVal
  | Add !Reg !RegOrVal
  | Mul !Reg !RegOrVal
  | Mod !Reg !RegOrVal
  | Rcv !RegOrVal
  | Jgz !RegOrVal !RegOrVal
  deriving (Show, Eq)

someFunc :: IO ()
someFunc = do
  putStrLn "program"
  program <- V.fromList . fmap (parseInstruction . words)
             <$> readInputLines
  traverse_ print program
  putStrLn "result"
  print $ runMachine program

debugState :: State -> String
debugState (State p rs c f) = show (rs, c, f, p V.! c)

initState :: Program -> State
initState p = State p (M.fromList $ (\c -> (c, 0)) <$> ['a'..'z']) 0 0

initState2 :: Program -> State2
initState2 p = State2 p (initPrgState 0) (initPrgState 1)
  where
  initPrgState pid =
    PrgState (M.insert 'p' pid (M.fromList $ (\c -> (c, 0)) <$> ['a'..'z']))
             0 [] 0

runMachine :: Program -> Int
runMachine = go . initState
  where
    go s = case iterState s of
      Left i -> i
      Right s' -> go s'

iterState2 :: State2 -> Either Int State2
iterState2 (State2 prog prgState1 prgState2) = iter' prgState1 prgState2
  where
  iter' (Waiting i) (Waiting _) = Left i
  iter' (Waiting i) (Running rs2 c2 q2 i2)) =
    let (maybe
    Right $ 
    State2 (Terminated i) (snd $ stepProg2 rs2 c2 q2 i2)
    -- need to wake up if necessary
  iter' (Running rs1 c1 q1 i1) (Waiting i)) =
    Right $ State2 (snd $ stepProg2 rs1 c1 q1 i1) (Waiting i)
    -- need to wake up if necessary


{-
stepProg2 (PrgState prog rs ctr q freq) = go rs ctr q freq
  where
  go rs ctr q freq =
    case V.unsafeIndex prog ctr of
      Snd rv   -> Right $ State prog rs (ctr + 1) (getRegOrVal rs rv)
      Set r rv -> Right $
        State prog (setReg rs r (getRegOrVal rs rv)) (ctr + 1) freq
      Add r rv -> Right $
        State prog (modReg rs r (+ getRegOrVal rs rv)) (ctr + 1) freq
      Mul r rv -> Right $
        State prog (modReg rs r (* getRegOrVal rs rv)) (ctr + 1) freq
      Mod r rv -> Right $
        State prog (modReg rs r (`mod` getRegOrVal rs rv)) (ctr + 1) freq
      Rcv rv   ->
        if getRegOrVal rs rv == 0
          then Right $ State prog rs (ctr + 1) freq
          else Left freq
      Jgz rv1 rv2 -> Right $ State
                       prog
                       rs
                       (ctr + (if getRegOrVal rs rv1 > 0
                          then getRegOrVal rs rv2
                          else 1
                       ))
                       freq
-}

iterState :: State -> Either Int State2
iterState (State prog rs ctr freq) = go rs ctr freq
  where
  go rs ctr freq =
    case V.unsafeIndex prog ctr of
      Snd rv   -> Right $ State prog rs (ctr + 1) (getRegOrVal rs rv)
      Set r rv -> Right $
        State prog (setReg rs r (getRegOrVal rs rv)) (ctr + 1) freq
      Add r rv -> Right $
        State prog (modReg rs r (+ getRegOrVal rs rv)) (ctr + 1) freq
      Mul r rv -> Right $
        State prog (modReg rs r (* getRegOrVal rs rv)) (ctr + 1) freq
      Mod r rv -> Right $
        State prog (modReg rs r (`mod` getRegOrVal rs rv)) (ctr + 1) freq
      Rcv rv   ->
        if getRegOrVal rs rv == 0
          then Right $ State prog rs (ctr + 1) freq
          else Left freq
      Jgz rv1 rv2 -> Right $ State
                       prog
                       rs
                       (ctr + (if getRegOrVal rs rv1 > 0
                          then getRegOrVal rs rv2
                          else 1
                       ))
                       freq

getRegOrVal :: Registers -> RegOrVal -> Int
getRegOrVal rs (Reg r) = getReg rs r
getRegOrVal rs (Val v) = v

getReg :: Registers -> Reg -> Int
getReg = flip (M.findWithDefault 0)

setReg :: Registers -> Reg -> Int -> Registers
setReg rs r i = M.insert r i rs

modReg :: Registers -> Reg -> (Int -> Int) -> Registers
modReg rs r f = M.adjust f r rs

parseInstruction :: [String] -> Instruction
parseInstruction ["snd", rv] = Snd (parseRV rv)
parseInstruction ["rcv", rv] = Rcv (parseRV rv)
parseInstruction ["jgz", rv1, rv2] = Jgz (parseRV rv1) (parseRV rv2)
parseInstruction [instr, [r], rv] = instr' r (parseRV rv)
  where instr' = case instr of
                  "set" -> Set
                  "add" -> Add
                  "mul" -> Mul
                  "mod" -> Mod

parseRV :: String -> RegOrVal
parseRV [c] | isAlpha c = Reg c
parseRV s = Val (read s)

readInput :: IO String
readInput = readFile "input"

readInputLines :: IO [String]
readInputLines = lines <$> readInput
