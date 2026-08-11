{-# LANGUAGE ApplicativeDo #-}

module Mambda.Game (
    State,
    init,
    step,
    render,
    Render (..),
    Glyph (..),
    SnakeDirection,
    up,
    down,
    left,
    right,
    control,
) where

import Prelude hiding (init)

import Aztecs qualified
import Aztecs.ECS.World qualified as World
import Data.Functor (void)
import Data.Typeable (Typeable)
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
    deriving stock (Show, Eq)
    deriving anyclass (Aztecs.Component m)

newtype Velocity = Velocity Space
    deriving stock (Show)
    deriving anyclass (Aztecs.Component m)

newtype Renderable = Renderable Glyph
    deriving stock (Show)
    deriving anyclass (Aztecs.Component m)

newtype Collidable m = Collidable (Collision m)
    deriving anyclass (Aztecs.Component m)

data Collision m = forall a. (Aztecs.Component m a) => Collision a

type Ticks = Integer

newtype Lifetime = Lifetime Ticks
    deriving stock (Show)
    deriving anyclass (Aztecs.Component m)

newtype SnakeDirection = SnakeDirection Space

up :: SnakeDirection
up = SnakeDirection $ V2 (-1) 0

down :: SnakeDirection
down = SnakeDirection $ V2 1 0

left :: SnakeDirection
left = SnakeDirection $ V2 0 (-1)

right :: SnakeDirection
right = SnakeDirection $ V2 0 1

isDead :: Lifetime -> Bool
isDead (Lifetime x) = x <= 0

snakeHead :: (Monad m) => Aztecs.BundleT m
snakeHead =
    Aztecs.bundle (SnakeHead 3)
        <> Aztecs.bundle (Position (V2 1 1))
        <> Aztecs.bundle (Velocity (V2 0 0))
        <> Aztecs.bundle (Renderable Snake)

snakeSegment :: (Monad m) => Space -> Ticks -> Aztecs.BundleT m
snakeSegment pos lifetime =
    Aztecs.bundle (Position pos)
        <> Aztecs.bundle (Renderable SnakeSegment)
        <> Aztecs.bundle (Lifetime lifetime)

sampleWorld :: forall m. (Monad m, Typeable m) => Aztecs.Access m ()
sampleWorld = do
    void $ Aztecs.spawn snakeHead
    void $ Aztecs.spawn $ Aztecs.bundle (Position (V2 10 10)) <> Aztecs.bundle (Collidable $ Collision @m (Position (V2 2 2))) <> Aztecs.bundle (Renderable Wall)
    void $ Aztecs.spawn $ Aztecs.bundle (World (V2 20 20))

init :: (Monad m, Typeable m) => m (State m)
init = State . snd <$> Aztecs.runAccess sampleWorld World.empty

gameStep :: (Monad m, Typeable m) => Aztecs.Access m ()
gameStep = do
    snakeGhostSystem
    moveSystem
    collisionSystem
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
    dead <- fmap (Vector.filter (isDead . snd)) $ Aztecs.system $ Aztecs.runQuery $ (,) <$> Aztecs.entity <*> Aztecs.queryMap (\(Lifetime x) -> Lifetime $ x - 1)
    Vector.forM_ dead $ \(entityId, _) -> Aztecs.despawn entityId

collisionSystem :: forall m. (Monad m, Typeable m) => Aztecs.Access m ()
collisionSystem = do
    snakes <- Aztecs.system $ Aztecs.runQueryFiltered ((,) <$> Aztecs.entity <*> Aztecs.query @_ @Position) $ Aztecs.with @m @SnakeHead
    Vector.forM_ snakes $ \(snakeEntityId, position) -> do
        collisions <- Aztecs.system $ Aztecs.runQuery (findCollisions position)
        Vector.forM_ collisions $ \(Collidable (Collision collision)) ->
            Aztecs.insert snakeEntityId $ Aztecs.bundle collision
  where
    findCollisions :: Position -> Aztecs.Query m (Collidable m)
    findCollisions pos =
        Aztecs.query @m @(Collidable m) <* Aztecs.queryFilter (pos ==) (Aztecs.query @m @Position)

step :: (Monad m, Typeable m) => State m -> m (State m)
step (State world) = State . snd <$> Aztecs.runAccess gameStep world

control :: (Monad m) => SnakeDirection -> State m -> m (State m)
control (SnakeDirection dir) (State world) = State . snd <$> Aztecs.runAccess control' world
  where
    control' =
        Aztecs.system $ Aztecs.runQuery $ (,) <$> Aztecs.query @_ @SnakeHead <*> Aztecs.queryMap (\_ -> Velocity dir)

render :: forall m. (Monad m) => State m -> m Render
render (State world) = fst <$> Aztecs.runAccess doRender world

doRender :: forall m. (Monad m) => Aztecs.Access m Render
doRender = do
    World (V2 width height) <- fmap Vector.last $ Aztecs.system $ Aztecs.runQuery $ Aztecs.query @m @World
    positions <- Aztecs.system $ Aztecs.runQuery $ (,) <$> Aztecs.query <*> Aztecs.query
    let emptyGrid = Vector.replicate (fromInteger width) $ Vector.replicate (fromInteger height) Empty
        fullGrid = Vector.foldr writeElem emptyGrid positions
    pure $ Render fullGrid

writeElem :: (Position, Renderable) -> Vector.Vector (Vector.Vector Glyph) -> Vector.Vector (Vector.Vector Glyph)
writeElem (Position (V2 x y), Renderable glyph) grid =
    let row = grid Vector.! fromInteger x
        newRow = row Vector.// [(fromInteger y, glyph)]
     in grid Vector.// [(fromInteger x, newRow)]
