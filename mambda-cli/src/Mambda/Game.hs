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
import Data.Word (Word8)
import Linear

newtype State m = State {world :: Aztecs.World m}

newtype Render = Render (Vector.Vector (Vector.Vector Glyph))

data Glyph = Snake | SnakeSegment | Empty | Wall
    deriving stock (Show)

type Space = V2 Integer

newtype World = World Space
    deriving stock (Show)
    deriving anyclass (Aztecs.Component m)

newtype SnakeHead = SnakeHead {length :: Word8}
    deriving stock (Show)
    deriving anyclass (Aztecs.Component m)

newtype Position = Position Space
    deriving stock (Show)
    deriving anyclass (Aztecs.Component m)

newtype Velocity = Velocity Space
    deriving stock (Show)
    deriving anyclass (Aztecs.Component m)

newtype Renderable = Renderable Glyph
    deriving stock (Show)
    deriving anyclass (Aztecs.Component m)

type Ticks = Integer

newtype Lifetime = Lifetime Ticks
    deriving stock (Show)
    deriving anyclass (Aztecs.Component m)

snakeHead :: (Monad m) => Aztecs.BundleT m
snakeHead =
    Aztecs.bundle (SnakeHead 3)
        <> Aztecs.bundle (Position (V2 1 1))
        <> Aztecs.bundle (Velocity (V2 1 0))
        <> Aztecs.bundle (Renderable Snake)

snakeSegment :: (Monad m) => Space -> Ticks -> Aztecs.BundleT m
snakeSegment pos lifetime =
    Aztecs.bundle (Position pos)
        <> Aztecs.bundle (Renderable SnakeSegment)
        <> Aztecs.bundle (Lifetime lifetime)

sampleWorld :: (Monad m) => Aztecs.Access m ()
sampleWorld = do
    void $ Aztecs.spawn snakeHead
    void $ Aztecs.spawn $ Aztecs.bundle (World (V2 20 20))

init :: (Monad m) => m (State m)
init = State . snd <$> Aztecs.runAccess sampleWorld World.empty

gameStep :: (Monad m) => Aztecs.Access m ()
gameStep = do
    snakeGhostSystem
    moveSystem
    lifetimeSystem

moveSystem :: (Monad m) => Aztecs.Access m ()
moveSystem = void $ Aztecs.system $ Aztecs.runQuery $ Aztecs.queryMapWith move Aztecs.query
  where
    move (Velocity v) (Position pos) = Position $ min (pos + v) (V2 19 19)

snakeGhostSystem :: (Monad m) => Aztecs.Access m ()
snakeGhostSystem = do
    snakes <- Aztecs.system $ Aztecs.runQuery $ (,) <$> Aztecs.query <*> Aztecs.query
    Vector.forM_ snakes $ \(SnakeHead l, Position pos) -> Aztecs.spawn_ $ snakeSegment pos $ toInteger l

lifetimeSystem :: (Monad m) => Aztecs.Access m ()
lifetimeSystem = do
    void $ Aztecs.system $ Aztecs.runQuery $ Aztecs.queryMap (\(Lifetime x) -> Lifetime $ x - 1)
    dead <- Aztecs.system $ Aztecs.runQuery $ Aztecs.entity <* Aztecs.queryFilter (\(Lifetime x) -> x == 0) Aztecs.query
    Vector.forM_ dead $ \entityId -> Aztecs.despawn entityId

step :: (Monad m) => State m -> m (State m)
step (State world) = State . snd <$> Aztecs.runAccess gameStep world

render :: forall m. (Monad m) => State m -> m Render
render (State world) = fst <$> Aztecs.runAccess doRender world

doRender :: forall m. (Monad m) => Aztecs.Access m Render
doRender = do
    World (V2 width height) <- fmap Vector.last $ Aztecs.system $ Aztecs.runQuery $ Aztecs.query @m @World

    let foo :: Position -> Renderable -> (Position, Renderable)
        foo = (,)
    positions <- Aztecs.system $ Aztecs.runQuery $ foo <$> Aztecs.query <*> Aztecs.query
    let emptyGrid = Vector.replicate (fromInteger width) $ Vector.replicate (fromInteger height) Empty
        fullGrid = Vector.foldr writeElem emptyGrid positions
    pure $ Render fullGrid

writeElem :: (Position, Renderable) -> Vector.Vector (Vector.Vector Glyph) -> Vector.Vector (Vector.Vector Glyph)
writeElem (Position (V2 x y), Renderable glyph) grid =
    let row = grid Vector.! fromInteger x
        newRow = row Vector.// [(fromInteger y, glyph)]
     in grid Vector.// [(fromInteger x, newRow)]
