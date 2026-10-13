module Mambda.Movement (
  Velocity(..),
  Direction(..),
  Positioned(..),
  Moving(..),
  up,
  down,
  left,
  right,
  moving,
  positioned,
  moveSystem
) where

import Prelude

import Aztecs qualified
import Linear

import Mambda.Time (Time)
import Mambda.Time qualified as Time

type Space = V2 Integer

newtype Direction = Direction Space

up :: Direction
up = Direction $ V2 (-1) 0

down :: Direction
down = Direction $ V2 1 0

left :: Direction
left = Direction $ V2 0 (-1)

right :: Direction
right = Direction $ V2 0 1

data Velocity = Velocity
  { direction :: Direction
  , period :: Time
  }

newtype Positioned = Positioned Space
  deriving newtype (Eq)
  deriving anyclass (Aztecs.Component m)

data Moving = Moving
  { velocity :: Velocity
  , nextMove :: Time
  }
  deriving anyclass (Aztecs.Component m)

positioned :: Monad m => Space -> Aztecs.BundleT m
positioned = Aztecs.bundle . Positioned

moving :: Monad m => Time.Clock -> Space -> Velocity -> Aztecs.BundleT m
moving clock position vel@Velocity{period} =
  positioned position
    <> Aztecs.bundle (Moving vel $ Time.schedule period clock)

moveSystem :: Monad m => Time.Clock -> Aztecs.Access m ()
moveSystem clock = do
  _ <- Aztecs.system $ Aztecs.runQuery $ Aztecs.queryMapWith move' Aztecs.query
  _ <- Aztecs.system $ Aztecs.runQuery $ Aztecs.queryMap nextMove
  pure ()
  where
    move' m@(Moving (Velocity dir _) _) p@(Positioned x)
      | shouldMove m = Positioned $ move dir x
      | otherwise = p
    shouldMove (Moving _ nextMove) = nextMove <= Time.time clock
    nextMove moving@Moving{velocity = Velocity{period}}
      | shouldMove moving = moving{ nextMove = Time.schedule period clock}
      | otherwise = moving

move :: Direction -> Space -> Space
move (Direction dir) x = dir + x
