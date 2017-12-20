{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Lib
    ( someFunc
    ) where

import GHC.Generics (Generic)
import Control.Arrow ((&&&))
import Control.DeepSeq (NFData, deepseq)
import Data.Maybe (isJust, catMaybes)
import Data.Foldable (traverse_)
import Data.List (minimumBy, elemIndex, nubBy)
import Data.List.Extra (splitOn)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Debug.Trace

data Particle = Particle !Position !Velocity !Acceleration
  deriving (Show, Eq, Ord, Generic, NFData)

type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)
type Acceleration = (Int, Int, Int)

parseParticle :: String -> Particle
parseParticle = toParticle . fmap parseStat . words
  where
  toParticle [p, v, a] = Particle p v a
  parseStat = tuplify . fmap (read . filter (/= '>')) . splitOn "," . drop 3
    where tuplify (x:y:z:_) = (x,y,z)

getPosition :: Particle -> Position
getPosition (Particle p _ _) = p

distance :: (Int, Int, Int) -> Int
distance (x, y, z) = abs x + abs y + abs z

-- remove Collisions and iterate n times
collisionSim :: Int -> [Particle] -> [Particle]
collisionSim n particles =
  iterate (fmap iterParticle . removeCollisions) particles !! n

removeCollisions :: [Particle] -> [Particle]
removeCollisions particles = particles `deepseq`
  let particleTuples = (getPosition &&& Just) <$> particles
      positionMap = M.fromListWith (\_ _ -> Nothing) particleTuples
      annotatedTuples = M.toList positionMap
  in  catMaybes $ snd <$> annotatedTuples

iterParticle :: Particle -> Particle
iterParticle (Particle (px, py, pz) (vx, vy, vz) a@(ax, ay, az)) =
  Particle (px + vx + ax, py + vy + ay, pz + vz + az) (vx + ax, vy + ay, vz + az) a

someFunc :: IO ()
someFunc = do
  particles <- fmap parseParticle . lines <$> readFile "input"

  --part1
  let minParticle =
        minimumBy (comparing (\(Particle p v a) -> distance <$> [a, v, p]))
                  particles
      minIndex = elemIndex minParticle particles 
  print minParticle
  print minIndex 

  --part2
  print . length $ collisionSim 10000 particles
