module Mambda.Game (
    State,
    init,
    step,
    render,
    Render (..),
    Glyph (..),
) where

import Prelude hiding (init)

import Aztecs qualified
import Aztecs.ECS.World qualified as World
import Data.Functor (void)
import Data.Vector qualified as Vector
import Linear

newtype State m = State {world :: Aztecs.World m}

newtype Render = Render (Vector.Vector (Vector.Vector Glyph))

data Glyph = Snake | Empty | Wall

type Space = V2 Integer

newtype World = World Space
    deriving stock (Show)
    deriving anyclass (Aztecs.Component m)

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
snakeHead = Aztecs.bundle (Position (V2 1 1)) <> Aztecs.bundle (Velocity (V2 1 0))

sampleWorld :: (Monad m) => Aztecs.Access m ()
sampleWorld = do
    void $ Aztecs.spawn snakeHead
    void $ Aztecs.spawn $ Aztecs.bundle (World (V2 20 20))

init :: (Monad m) => m (State m)
init = State . snd <$> Aztecs.runAccess sampleWorld World.empty

gameStep :: (Monad m) => Aztecs.Access m ()
gameStep =
    void $ Aztecs.system $ Aztecs.runQuery $ Aztecs.queryMapWith move Aztecs.query
  where
    move (Velocity v) (Position pos) = Position $ min (pos + v) (V2 19 19)

step :: (Monad m) => State m -> m (State m)
step (State world) = State . snd <$> Aztecs.runAccess gameStep world

render :: forall m. (Monad m) => State m -> m Render
render (State world) = fst <$> Aztecs.runAccess doRender world

doRender :: forall m. (Monad m) => Aztecs.Access m Render
doRender = do
    World (V2 width height) <- fmap Vector.last $ Aztecs.system $ Aztecs.runQuery $ Aztecs.query @m @World
    positions <- Aztecs.system $ Aztecs.runQuery $ Aztecs.query @m @Position
    let emptyGrid = Vector.replicate (fromInteger width) $ Vector.replicate (fromInteger height) Empty
        fullGrid = Vector.foldr (writeElem Snake) emptyGrid positions
    pure $ Render fullGrid

writeElem :: a -> Position -> Vector.Vector (Vector.Vector a) -> Vector.Vector (Vector.Vector a)
writeElem a (Position (V2 x y)) grid =
    let row = grid Vector.! fromInteger x
        newRow = row Vector.// [(fromInteger y, a)]
     in grid Vector.// [(fromInteger x, newRow)]
