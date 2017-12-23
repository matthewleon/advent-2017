{-# LANGUAGE RecordWildCards #-}

module Lib
    ( someFunc
    ) where

import Control.Monad.ST (ST, runST)
import Data.Char (ord, isAlpha)
import Data.Numbers.Primes (isPrime)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Map.Strict as M

import Debug.Trace

type Program = V.Vector Instruction
type Registers h = MV.MVector h Int
type Reg = Char
type Counter = Int

data State h = State {
    regs :: Registers h
  , cntr :: !Counter
  , muls :: !Int
  }

data Instruction =
    Set !Reg !RegOrVal
  | Sub !Reg !RegOrVal
  | Mul !Reg !RegOrVal
  | Jnz !RegOrVal !RegOrVal
  deriving (Show, Eq)

data RegOrVal = Reg !Reg | Val !Int
  deriving (Show, Eq)
  
someFunc :: IO ()
someFunc = do
  prog <- parseInput <$> readFile "input"
  --print prog
  print $ runProg prog
  --print $ runProg2 prog
  print $ length $ filter (\i -> ((i - 108100) `mod` 17) == 0 && not (isPrime i))
                          [108100 .. 125100]

stateStr :: State h -> ST h String
stateStr State{..} = do
  rs <- UV.freeze regs
  pure $ show cntr ++ ": " ++ show muls ++ "    " ++
         show (zip ['a'..'h'] $ UV.toList rs)

runProg :: Program -> Int
runProg prog = runST $ do
  s <- initState
  go s
  where
  go s = do
    es <- iterState prog s
    case es of
      Left i -> pure i
      Right s' -> go s'

runProg2 :: Program -> Int
runProg2 prog = runST $ do
  s <- initDebugState
  unsafeWriteReg (regs s) 'a' 1
  go s
  where
  go s = do
    es <- iterState prog s
    case es of
      Left i -> pure i
      Right s' -> do
        traceShowM =<< stateStr s'
        go s'

initState :: ST h (State h)
initState = do
  iv <- MV.replicate (ord 'h' + 1 - ord 'a') 0
  pure $ State iv 0 0

initDebugState :: ST h (State h)
initDebugState = do
  iv <- UV.thaw $ UV.fromList [1, 108117, 125100, 2, 2, 1, 0, 1]
  pure $ State iv 11 0

unsafeReadReg :: Registers h -> Reg -> ST h Int
unsafeReadReg rs r = MV.unsafeRead rs (ord r - ord 'a')

unsafeWriteReg :: Registers h -> Reg -> Int -> ST h ()
unsafeWriteReg rs r = MV.unsafeWrite rs (ord r - ord 'a')

unsafeModReg :: Registers h -> (Int -> Int) -> Reg -> ST h ()
unsafeModReg rs f r = MV.unsafeModify rs f (ord r - ord 'a')

parseInput :: String -> Program
parseInput = V.fromList . fmap (parseInstruction . words) . lines

parseInstruction :: [String] -> Instruction
parseInstruction ["set", [r], rv] = Set r (parseRV rv) 
parseInstruction ["sub", [r], rv] = Sub r (parseRV rv) 
parseInstruction ["mul", [r], rv] = Mul r (parseRV rv) 
parseInstruction ["jnz", rv1, rv2] = Jnz (parseRV rv1) (parseRV rv2)

parseRV :: String -> RegOrVal
parseRV [c] | isAlpha c = Reg c
parseRV s = Val (read s)

iterState :: Program -> State h -> ST h (Either Int (State h))
iterState prog State{..} =
  case prog V.!? cntr of
    Nothing -> pure $ Left muls
    Just (Set r rv) -> do
      i <- getRV rv
      unsafeWriteReg regs r i
      pure . Right $ State regs (cntr + 1) muls
    Just (Sub r rv) -> do
      i <- getRV rv
      unsafeModReg regs (\x -> x - i) r
      pure . Right $ State regs (cntr + 1) muls
    Just (Mul r rv) -> do
      i <- getRV rv
      unsafeModReg regs (* i) r
      pure . Right $ State regs (cntr + 1) (muls + 1)
    Just (Jnz rv1 rv2) -> do
      x <- getRV rv1
      y <- getRV rv2
      pure . Right $ if x == 0
        then State regs (cntr + 1) muls
        else State regs (cntr + y) muls
  where
  getRV (Val v) = pure v
  getRV (Reg r) = unsafeReadReg regs r
