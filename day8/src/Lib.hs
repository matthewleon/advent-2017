module Lib where

import Data.Foldable (foldl')
import qualified Data.Map.Strict as M

data Instruction = Instruction {
  _modReg :: Register
, _eff    :: RegEffect
, _amount :: Int
, _cond   :: Condition
}
  deriving (Show)

data RegEffect = Inc | Dec
  deriving (Show, Eq)

data Condition = Condition {
  _condReg :: Register
, _comp    :: Comparator
, _comVal  :: Int
}
  deriving (Show)

type Register = String

data Comparator = Greater | GreaterEq | Less | LessEq | Equals | NotEq
  deriving (Show, Eq)

parseInstruction :: String -> Instruction
parseInstruction s = case words s of
  (modReg : effStr : amountStr : _ : condRegStr : compStr : comValStr : _) ->
    Instruction {
      _modReg = modReg
    , _eff = if effStr == "inc" then Inc else Dec
    , _amount = read amountStr
    , _cond = Condition {
        _condReg = condRegStr
      , _comp    = case compStr of
                     ">"  -> Greater
                     ">=" -> GreaterEq
                     "<"  -> Less
                     "<=" -> LessEq
                     "==" -> Equals
                     "!=" -> NotEq
                     _    -> error $ "invalid comparator: " ++ compStr
      , _comVal  = read comValStr
      }
    }
  _ -> error $ "unsupported instruction format: " ++ s

initRegisters :: [Instruction] -> M.Map Register Int
initRegisters instrs =
  let allRegisters =
        concatMap (\instr -> [_modReg instr, _condReg (_cond instr)]) instrs
  in foldl' (\m reg -> M.insert reg 0 m) M.empty allRegisters

run :: [Instruction] -> M.Map Register Int
run instructions = foldl' runInstr (initRegisters instructions) instructions
  where
  runInstr :: M.Map Register Int -> Instruction -> M.Map Register Int
  runInstr regMap instr = if conditionMet then runInstr' else regMap
    where
    runInstr' = M.adjust
                  (\x -> (if _eff instr == Inc then (+) else (-)) x (_amount instr))
                  (_modReg instr)
                  regMap

    conditionMet = (compToFn . _comp $ _cond instr)
                     ((M.!) regMap (_condReg $ _cond instr))
                     (_comVal $ _cond instr)
      where
        compToFn :: Comparator -> Int -> Int -> Bool
        compToFn Greater   = (>)
        compToFn GreaterEq = (>=)
        compToFn Less      = (<)
        compToFn LessEq    = (<=)
        compToFn Equals    = (==)
        compToFn NotEq     = (/=)

largestVal :: M.Map Register Int -> Int
largestVal = maximum . fmap snd . M.toList
