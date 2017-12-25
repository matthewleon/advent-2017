module Lib
    ( someFunc
    ) where

data TMState = TMState ![Bool] ![Bool] !State !Int
  deriving (Show)

type Steps = Int
data Transition = Transition !Bool !Direction !State
  deriving (Show)
data Direction = L | R
  deriving (Show, Eq)
data State = A | B | C | D | E | F
  deriving (Show)

someFunc :: IO ()
someFunc = print $ getOnes $ iterate iter initTMState !! 12317297
--someFunc = print $ getOnes $ iterate iterDemo initTMState !! 6
--1449095
--1449094 (too high still)

getOnes :: TMState -> Int
getOnes (TMState _ _ _ i) = i

initTMState :: TMState
initTMState = TMState (repeat False) (repeat False) A 0

iter :: TMState -> TMState
iter (TMState (p:prev) (v:next) state ones) =
  let (Transition v' direction state') = transition state v
      (prev', next') = if direction == L
                         then (prev, p:v':next)
                         else (v':p:prev, next)
      ones' = ones + if v == v'
                      then 0
                      else if v' then 1 else -1
  in TMState prev' next' state' ones'

iterDemo :: TMState -> TMState
iterDemo (TMState (p:prev) (v:next) state ones) =
  let (Transition v' direction state') = demoTransition state v
      (prev', next') = if direction == L
                         then (prev, p:v':next)
                         else (v':p:prev, next)
      ones' = ones + if v == v'
                      then 0
                      else if v' then 1 else -1
  in TMState prev' next' state' ones'

demoTransition :: State -> Bool -> Transition
demoTransition A False = Transition True R B
demoTransition A True = Transition False L B
demoTransition B False = Transition True L A
demoTransition B True = Transition True R A

transition :: State -> Bool -> Transition
transition A False = Transition True R B
transition A True = Transition False L D
transition B False = Transition True R C
transition B True = Transition False R F
transition C False = Transition True L C
transition C True = Transition True L A
transition D False = Transition False L E
transition D True = Transition True R A
transition E False = Transition True L A
transition E True = Transition False R B
transition F False = Transition False R C
transition F True = Transition False R E
