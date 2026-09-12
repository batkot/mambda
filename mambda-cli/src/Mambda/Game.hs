{-# LANGUAGE ApplicativeDo #-}

module Mambda.Game (
    WorldSettings (..),
    State,
    init,
    step,
    render,
    laser,
    Render (..),
    Glyph (..),
    PlayerInput (..),
    up,
    down,
    left,
    right,
    control,
    PlayerId (..),
) where

import Prelude hiding (init)

import Aztecs qualified
import Aztecs.ECS.World qualified as World
import Control.Monad
import Data.Bifunctor
import Data.Foldable (traverse_)
import Data.List.NonEmpty hiding (init)
import Data.Typeable (Typeable)
import Data.Vector qualified as Vector
import Data.Word (Word8)
import Linear

newtype State m = State (Aztecs.World m)

newtype Render = Render (Vector.Vector (Vector.Vector Glyph))

data Glyph
    = Snake
    | SnakeSegment
    | Empty
    | Wall
    | Portal
    | Apple
    | GoldenApple
    | Poison
    | Laser
    deriving stock (Show)

type Space = V2 Integer

data PlayerId = One | Two
    deriving stock (Show, Eq)

newtype World = World Space
    deriving stock (Show)
    deriving anyclass (Aztecs.Component m)

data SnakeHead = SnakeHead
    { playerId :: PlayerId
    , length :: Word8
    }
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

newtype Laser = LaserBeam ()
    deriving stock (Show)
    deriving anyclass (Aztecs.Component m)

newtype Collidable m = Collidable (Collision m, Collision m)
    deriving anyclass (Aztecs.Component m)

newtype Grow = Grow (Integer, Bool)
    deriving stock (Show)

data NoOp = NoOp
    deriving stock (Show)
    deriving anyclass (Aztecs.Component m)

data Dead = Dead
    deriving stock (Show)

instance (Monad m) => Aztecs.Component m Dead where
    componentOnInsert entity Dead = Aztecs.despawn entity

instance (Monad m) => Aztecs.Component m Grow where
    componentOnInsert _ (Grow (_, True)) = pure ()
    componentOnInsert entity (Grow (size, False)) = do
        headMaybe <- Aztecs.lookup @_ @SnakeHead entity
        forM_ headMaybe $ \(SnakeHead player score) ->
            Aztecs.insertUntracked entity $ Aztecs.bundle $ SnakeHead player $ score + fromInteger size
        snakeSegments <- maybe mempty Aztecs.unChildren <$> Aztecs.lookup entity
        let bumpLifetime entityId = do
                currentLifetime <- maybe 0 (\(Lifetime x) -> x) <$> Aztecs.lookup entityId
                Aztecs.insertUntracked entityId $ Aztecs.bundle $ Lifetime $ currentLifetime + fromInteger size
        traverse_ bumpLifetime snakeSegments
        Aztecs.insert entity $ Aztecs.bundle (Grow (size, True))
    componentOnChange _ _ (Grow (_, True)) = pure ()
    componentOnChange entity _ (Grow (size, False)) = do
        headMaybe <- Aztecs.lookup @_ @SnakeHead entity
        forM_ headMaybe $ \(SnakeHead player score) ->
            Aztecs.insertUntracked entity $ Aztecs.bundle $ SnakeHead player $ score + fromInteger size
        snakeSegments <- maybe mempty Aztecs.unChildren <$> Aztecs.lookup entity
        let bumpLifetime entityId = do
                currentLifetime <- maybe 0 (\(Lifetime x) -> x) <$> Aztecs.lookup entityId
                Aztecs.insertUntracked entityId $ Aztecs.bundle $ Lifetime $ currentLifetime + fromInteger size
        traverse_ bumpLifetime snakeSegments
        Aztecs.insert entity $ Aztecs.bundle (Grow (size, True))

data Collision m = forall a. (Aztecs.Component m a) => Collision a

type Ticks = Integer

newtype Lifetime = Lifetime Ticks
    deriving stock (Show)
    deriving anyclass (Aztecs.Component m)

newtype SnakeDirection = SnakeDirection Space

data Eaten = Eaten

instance (Monad m, Typeable m) => Aztecs.Component m Eaten where
    componentOnInsert entityId _ = do
        takenSpots <- Aztecs.system $ Aztecs.runQuery $ Aztecs.query @m @Position
        World (V2 width height) <- Aztecs.system $ Aztecs.runQuerySingle $ Aztecs.query @m @World
        Aztecs.insert entityId $ Aztecs.bundle @m Dead
        let free = [Position (V2 w h) | w <- [0 .. width], h <- [0 .. height], Vector.notElem (Position (V2 w h)) takenSpots]
        case free of
            [] -> pure ()
            ((Position x) : _) -> Aztecs.spawn_ $ apple x

data PlayerInput = ChangeDirection PlayerId SnakeDirection

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

data WorldSettings = WorldSettings
    { width :: Word8
    , height :: Word8
    }

snakeHead :: (Monad m) => PlayerId -> Aztecs.BundleT m
snakeHead playerId =
    Aztecs.bundle (SnakeHead playerId 3)
        <> Aztecs.bundle startPos
        <> Aztecs.bundle (Velocity (V2 0 0))
        <> Aztecs.bundle (Renderable Snake)
  where
    startPos =
        case playerId of
            One -> Position (V2 1 1)
            Two -> Position (V2 19 19)

snakeSegment :: forall m. (Monad m, Typeable m) => Aztecs.EntityID -> Space -> Ticks -> Aztecs.BundleT m
snakeSegment snakeHeadId pos lifetime =
    Aztecs.bundle (Position pos)
        <> Aztecs.bundle (Renderable SnakeSegment)
        <> Aztecs.bundle (Collidable (Collision @m Dead, Collision @m NoOp))
        <> Aztecs.bundle (Lifetime lifetime)
        <> Aztecs.bundle (Aztecs.Parent snakeHeadId)

apple :: forall m. (Monad m, Typeable m) => Space -> Aztecs.BundleT m
apple pos =
    Aztecs.bundle (Position pos)
        <> Aztecs.bundle (Renderable Apple)
        <> Aztecs.bundle (Collidable (Collision @m (Grow (1, False)), Collision @m Eaten))

wall :: forall m. (Monad m, Typeable m) => Space -> Aztecs.Access m ()
wall pos = void $ Aztecs.spawn $ Aztecs.bundle (Position pos) <> Aztecs.bundle (Collidable (Collision @m Dead, Collision @m NoOp)) <> Aztecs.bundle (Renderable Wall)

sampleWorld :: forall m. (Monad m, Typeable m) => NonEmpty PlayerId -> WorldSettings -> Aztecs.Access m ()
sampleWorld players WorldSettings{width, height} = do
    forM_ players $ Aztecs.spawn . snakeHead
    void $ Aztecs.spawn $ apple (V2 1 5)
    void $ Aztecs.spawn $ Aztecs.bundle (Position (V2 10 10)) <> Aztecs.bundle (Collidable (Collision @m (Position (V2 2 2)), Collision @m NoOp)) <> Aztecs.bundle (Renderable Portal)
    void $ Aztecs.spawn $ Aztecs.bundle (Position (V2 10 19)) <> Aztecs.bundle (Collidable (Collision @m (Grow (-5, False)), Collision @m Dead)) <> Aztecs.bundle (Renderable Poison)
    void $ Aztecs.spawn $ Aztecs.bundle (World (V2 (toInteger height) (toInteger width)))
    forM_ walls $ \(x, y) -> wall $ V2 x y
    forM_ borders $ \(h, w) ->
        let (exitH, exitW) =
                case (h, w) of
                    (-1, y) -> (heightInt - 1, y)
                    (x, -1) -> (x, widthInt - 1)
                    (x, y) | x == heightInt -> (0, y)
                    (x, _) -> (x, 0)
         in void $ Aztecs.spawn $ Aztecs.bundle (Position (V2 h w)) <> Aztecs.bundle (Collidable (Collision @m (Position (V2 exitH exitW)), Collision @m NoOp))
  where
    widthInt = toInteger width
    heightInt = toInteger height
    walls =
        [ (h + x, w)
        | h <- [4, 12]
        , w <- [6, 14]
        , x <- [0 .. 3]
        ]
    borders =
        [ (h, w)
        | h <- [-1, 0 .. heightInt]
        , w <- [-1, 0 .. widthInt]
        , or [h == -1, h == heightInt, w == -1, w == widthInt]
        ]

init :: (Monad m, Typeable m) => NonEmpty PlayerId -> WorldSettings -> m (State m)
init players worldSettings = State . snd <$> Aztecs.runAccess (sampleWorld players worldSettings) World.empty

gameStep :: (Monad m, Typeable m) => Aztecs.Access m Bool
gameStep = do
    lifetimeSystem
    snakeGhostSystem
    moveSystem
    collisionSystem
    endGameSystem

moveSystem :: (Monad m) => Aztecs.Access m ()
moveSystem = void $ Aztecs.system $ Aztecs.runQuery $ Aztecs.queryMapWith move Aztecs.query
  where
    move (Velocity v) (Position pos) = Position $ pos + v

snakeGhostSystem :: (Monad m, Typeable m) => Aztecs.Access m ()
snakeGhostSystem = do
    snakes <- Aztecs.system $ Aztecs.runQuery $ Aztecs.queryFilter (\(_, _, _, Velocity v) -> v /= V2 0 0) $ (,,,) <$> Aztecs.entity <*> Aztecs.query <*> Aztecs.query <*> Aztecs.query
    Vector.forM_ snakes $ \(snakeEntityId, SnakeHead _ l, Position pos, _) -> Aztecs.spawn_ $ snakeSegment snakeEntityId pos $ toInteger l

lifetimeSystem :: (Monad m) => Aztecs.Access m ()
lifetimeSystem = do
    dead <- fmap (Vector.filter (isDead . snd)) $ Aztecs.system $ Aztecs.runQuery $ (,) <$> Aztecs.entity <*> Aztecs.queryMap (\(Lifetime x) -> Lifetime $ x - 1)
    Vector.forM_ dead $ \(entityId, _) -> Aztecs.despawn entityId

collisionSystem :: forall m. (Monad m, Typeable m) => Aztecs.Access m ()
collisionSystem = do
    snakes <- Aztecs.system $ Aztecs.runQueryFiltered ((,) <$> Aztecs.entity <*> Aztecs.query @_ @Position) $ Aztecs.with @m @SnakeHead
    Vector.forM_ snakes $ \(snakeEntityId, position) -> do
        collisions <- Aztecs.system $ Aztecs.runQuery (findCollisions position)
        Vector.forM_ collisions $ \(entityId, Collidable (Collision collision, Collision onHost)) -> do
            Aztecs.insert snakeEntityId $ Aztecs.bundle collision
            Aztecs.insert entityId $ Aztecs.bundle onHost

findCollisions :: forall m. (Monad m, Typeable m) => Position -> Aztecs.Query m (Aztecs.EntityID, Collidable m)
findCollisions pos = fmap fst $ Aztecs.queryFilter ((==) pos . snd) $ (,) <$> ((,) <$> Aztecs.entity <*> Aztecs.query @m @(Collidable m)) <*> Aztecs.query @m @Position

endGameSystem :: (Monad m) => Aztecs.Access m Bool
endGameSystem = fmap Vector.null $ Aztecs.system $ Aztecs.runQuery $ Aztecs.query @_ @SnakeHead

step :: (Monad m, Typeable m) => State m -> m (Bool, State m)
step (State world) = second State <$> Aztecs.runAccess gameStep world

control :: (Monad m) => PlayerInput -> State m -> m (State m)
control (ChangeDirection playerId (SnakeDirection dir)) (State world) = State . snd <$> Aztecs.runAccess control' world
  where
    mapVel (Velocity currentVel)
        | currentVel + dir == V2 0 0 = Velocity currentVel
        | otherwise = Velocity dir
    control' =
        Aztecs.system $ Aztecs.runQuery $ Aztecs.queryFilter (\(SnakeHead pId _, _) -> pId == playerId) ((,) <$> Aztecs.query @_ @SnakeHead <*> Aztecs.queryMap mapVel)

laser :: (Monad m, Typeable m) => State m -> m (State m)
laser (State w) = State . snd <$> Aztecs.runAccess doLaser w
  where
    doLaser = do
        (World (V2 width height)) <- Aztecs.system $ Aztecs.runQuerySingle Aztecs.query
        snakes <- Aztecs.system $ Aztecs.runQuery $ (,,) <$> Aztecs.query @_ @SnakeHead <*> Aztecs.query @_ @Position <*> Aztecs.query @_ @Velocity
        forM_ snakes $ \(_, Position (V2 px py), Velocity (V2 vx vy)) ->
            forM_ (Vector.generate (fromInteger $ max width height) (\x -> V2 (max 0 (min (width - 1) (px + toInteger x * vx))) (max 0 (min (height - 1) (py + toInteger x * vy))))) $ \laserPos -> do
                Aztecs.spawn_ $
                    Aztecs.bundle (Position laserPos)
                        <> Aztecs.bundle (LaserBeam ())
                        <> Aztecs.bundle (Renderable Laser)
                        <> Aztecs.bundle (Lifetime 1)
                collisions <- Aztecs.system $ Aztecs.runQuery (findCollisions $ Position laserPos)
                Vector.forM_ collisions $ \(entityId, _) ->
                    Aztecs.insert entityId $ Aztecs.bundle $ Lifetime 0

render :: forall m. (Monad m) => State m -> m Render
render (State world) = fst <$> Aztecs.runAccess doRender world

doRender :: forall m. (Monad m) => Aztecs.Access m Render
doRender = do
    World (V2 width height) <- Aztecs.system $ Aztecs.runQuerySingle $ Aztecs.query @m @World
    positions <- Aztecs.system $ Aztecs.runQuery $ (,) <$> Aztecs.query <*> Aztecs.query
    let emptyGrid = Vector.replicate (fromInteger width) $ Vector.replicate (fromInteger height) Empty
        fullGrid = Vector.foldr writeElem emptyGrid positions
    pure $ Render fullGrid

writeElem :: (Position, Renderable) -> Vector.Vector (Vector.Vector Glyph) -> Vector.Vector (Vector.Vector Glyph)
writeElem (Position (V2 x y), Renderable glyph) grid =
    let row = grid Vector.! fromInteger x
        newRow = row Vector.// [(fromInteger y, glyph)]
     in grid Vector.// [(fromInteger x, newRow)]
