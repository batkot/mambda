module Mambda.Game where

import Prelude

import Aztecs qualified
import Aztecs.ECS.World qualified as World
import Data.Functor (void)
import Data.Vector qualified as Vector
import Linear

newtype State m = State {world :: Aztecs.World m}

type Space = V2 Integer

newtype Position = Position Space
    deriving stock (Show)
    deriving anyclass (Aztecs.Component m)

newtype Velocity = Velocity Space
    deriving stock (Show)
    deriving anyclass (Aztecs.Component m)

type Ticks = Word

newtype Lifetime = Lifetime Ticks
    deriving stock (Show)
    deriving anyclass (Aztecs.Component m)

snakeHead :: (Monad m) => Aztecs.BundleT m
snakeHead = Aztecs.bundle (Position (V2 5 5)) <> Aztecs.bundle (Velocity (V2 1 0))

sampleWorld :: (Monad m) => Aztecs.Access m ()
sampleWorld =
    void $ Aztecs.spawn snakeHead

initWorld :: (Monad m) => m (State m)
initWorld = State . snd <$> Aztecs.runAccess sampleWorld World.empty

gameStep :: (Monad m) => Aztecs.Access m ()
gameStep = pure ()

step :: (Monad m) => State m -> m (State m)
step (State world) = State . snd <$> Aztecs.runAccess gameStep world

foo :: forall m. (Monad m) => Aztecs.Access m (Vector.Vector Position)
foo = Aztecs.system $ Aztecs.runQuery $ Aztecs.query @m @Position
