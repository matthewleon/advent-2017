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
  deriving (Show)

data PrgState = Running !Registers !Counter ![Int] !Int
              | Waiting !Registers !Counter !Int
  deriving (Show)

data RegOrVal = Reg !Reg | Val !Int
  deriving (Show, Eq)
data Instruction =
    Snd !Reg
  | Set !Reg !RegOrVal
  | Add !Reg !RegOrVal
  | Mul !Reg !RegOrVal
  | Mod !Reg !RegOrVal
  | Rcv !Reg
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

  testProgram <- V.fromList . fmap (parseInstruction . words) . lines <$> readFile "testinput"
  print $ runMachine2 testProgram
  print $ runMachine2 program

debugState :: State -> String
debugState (State p rs c f) = show (rs, c, f, p V.! c)

initState :: Program -> State
initState p = State p (M.fromList $ (\c -> (c, 0)) <$> ['a'..'z']) 0 0

initState2 :: Program -> State2
initState2 p = State2 p (initPrgState 0) (initPrgState 1)
  where
  initPrgState pid =
    Running (M.insert 'p' pid (M.fromList $ (\c -> (c, 0)) <$> ['a'..'z']))
             0 [] 0

runMachine :: Program -> Int
runMachine = go . initState
  where
    go s = case iterState s of
      Left i -> i
      Right s' -> go s'

runMachine2 :: Program -> Int
runMachine2 = go . initState2
  where
    go s = case iterState2 s of
      Left i -> i
      Right s'@(State2 _ s1 s2) ->
        --trace "state" $
        --trace (quickShow s1) $
        --trace (quickShow s2) $
        go $! s'

quickShow :: PrgState -> String
quickShow (Running _ ctr q i) = "Running " ++ unwords [show ctr, show q, show i]
quickShow (Waiting _ ctr i) = "Waiting " ++ unwords [show ctr, show i]

iterState2 :: State2 -> Either Int State2
iterState2 (State2 prog prgState1 prgState2) = iter' prgState1 prgState2
  where
  iter' :: PrgState -> PrgState -> Either Int State2
  iter' (Waiting _ _ sent) Waiting{} = Left sent
  iter' (Running rs1 c1 q1 i1) _ =
    let (maybeI, prgState1') = stepProg2 prog prgState1
    in  Right $ State2 prog prgState1'
        $ case maybeI of
            Just i -> enqueue prgState2 i
            Nothing -> prgState2
  iter' _ (Running rs2 c2 q2 i2) =
    let (maybeI, prgState2') = stepProg2 prog prgState2
    in  Right $ (\s -> State2 prog s prgState2')
        $ case maybeI of
            Just i -> enqueue prgState1 i
            Nothing -> prgState1

enqueue :: PrgState -> Int -> PrgState
enqueue (Running rs c is sent) i = Running rs c (is ++ [i]) sent
enqueue (Waiting rs c sent) i = Running rs c [i] sent

stepProg2 :: Program -> PrgState -> (Maybe Int, PrgState)
stepProg2 _ w@Waiting{} = (Nothing, w)
stepProg2 prog (Running rs ctr q sent) =
  let ctr' = ctr + 1
  in case V.unsafeIndex prog ctr of
       Snd r    -> (Just (getReg rs r), Running rs ctr' q (sent + 1))
       Set r rv -> (Nothing,
         Running (setReg rs r (getRegOrVal rs rv)) ctr' q sent)
       Add r rv -> (Nothing,
         Running (modReg rs r (+ getRegOrVal rs rv)) ctr' q sent)
       Mul r rv -> (Nothing,
         Running (modReg rs r (* getRegOrVal rs rv)) ctr' q sent)
       Mod r rv -> (Nothing,
         Running (modReg rs r (`mod` getRegOrVal rs rv)) ctr' q sent)
       Rcv r    -> case q of
                    i:is -> (Nothing, Running (setReg rs r i) ctr' is sent)
                    _    -> (Nothing, Waiting rs ctr sent)
       Jgz rv1 rv2 -> (Nothing,
         Running
          rs
          (ctr + (if getRegOrVal rs rv1 > 0
             then getRegOrVal rs rv2
             else 1
          ))
          q
          sent)

iterState :: State -> Either Int State
iterState (State prog rs ctr freq) = go rs ctr freq
  where
  go rs ctr freq =
    case V.unsafeIndex prog ctr of
      Snd r    -> Right $ State prog rs (ctr + 1) (getReg rs r)
      Set r rv -> Right $
        State prog (setReg rs r (getRegOrVal rs rv)) (ctr + 1) freq
      Add r rv -> Right $
        State prog (modReg rs r (+ getRegOrVal rs rv)) (ctr + 1) freq
      Mul r rv -> Right $
        State prog (modReg rs r (* getRegOrVal rs rv)) (ctr + 1) freq
      Mod r rv -> Right $
        State prog (modReg rs r (`mod` getRegOrVal rs rv)) (ctr + 1) freq
      Rcv r   ->
        if getReg rs r == 0
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
parseInstruction ["snd", [r]] = Snd r
parseInstruction ["rcv", [r]] = Rcv r
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
