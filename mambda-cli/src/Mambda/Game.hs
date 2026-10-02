{-# LANGUAGE ApplicativeDo #-}

module Mambda.Game (
    WorldSettings (..),
    State,
    init,
    step,
    render,
    Render (..),
    Glyph (..),
    PlayerInput (..),
    PlayerControls (..),
    InputQueue,
    addInput,
    initInputQueue,
    up,
    down,
    left,
    right,
    PlayerId (..),
) where

import Prelude hiding (init, (!!))

import Aztecs qualified
import Aztecs.ECS.World qualified as World
import Control.Monad
import Data.Bifunctor
import Data.Foldable (traverse_)
import Data.List.NonEmpty hiding (init)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import Data.Vector qualified as Vector
import Data.Word (Word8)
import Linear
import System.Random qualified as Random

newtype State m = State (Aztecs.World m)

newtype Render = Render (Vector.Vector (Vector.Vector Glyph))

data Glyph
    = Snake PlayerId
    | SnakeSegment PlayerId
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
    deriving stock (Show, Eq, Ord)

newtype Seed = Seed Integer
    deriving (Show)

foo :: Seed -> Ticks -> Seed
foo (Seed x) y = Seed $ x + y

data World = World
    { size :: Space
    , seed :: Seed
    , tick :: Ticks
    }
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

newtype NewCollision m = NewCollision (CollisionAction m)
    deriving anyclass (Aztecs.Component m)

type CollisionAction m = Aztecs.EntityID -> Aztecs.EntityID -> Aztecs.Access m ()

score :: (Monad m) => Aztecs.EntityID -> Scores -> Aztecs.Access m ()
score entity (Scores score') = do
    headMaybe <- Aztecs.lookup @_ @SnakeHead entity
    forM_ headMaybe $ \(SnakeHead player score) ->
        Aztecs.insertUntracked entity $ Aztecs.bundle $ SnakeHead player $ fromInteger $ max 1 $ toInteger score + score'
    snakeSegments <- maybe mempty Aztecs.unChildren <$> Aztecs.lookup entity
    let bumpLifetime entityId = do
            currentLifetime <- maybe 0 (\(Lifetime x) -> x) <$> Aztecs.lookup entityId
            Aztecs.insertUntracked entityId $ Aztecs.bundle $ Lifetime $ max 1 $ currentLifetime + fromInteger score'
    traverse_ bumpLifetime snakeSegments

spawnEntity :: forall m. (Monad m) => (Space -> Aztecs.BundleT m) -> Aztecs.Access m ()
spawnEntity toSpawn = do
    takenSpots <- Aztecs.system $ Aztecs.runQuery $ Aztecs.query @m @Position
    World{size = V2 width height, seed, tick} <- Aztecs.system $ Aztecs.runQuerySingle $ Aztecs.query @m @World
    let free = NonEmpty.nonEmpty [Position (V2 w h) | w <- [0 .. width], h <- [0 .. height], Vector.notElem (Position (V2 w h)) takenSpots]
    case free of
        Nothing -> pure ()
        Just available -> Aztecs.spawn_ $ toSpawn $ (\(Position x) -> x) $ pick (foo seed tick) available

pick :: forall a. Seed -> NonEmpty a -> a
pick (Seed seed) elements = elements !! index
  where
    index = fst $ Random.uniformR (0, NonEmpty.length elements) stdGen
    stdGen = Random.mkStdGen $ fromInteger seed

appleCollision :: (Monad m, Typeable m) => Scores -> CollisionAction m
appleCollision scores appleEntityId snakeEntityId = do
    score snakeEntityId scores
    void $ Aztecs.despawn appleEntityId
    spawnEntity apple

despawnCollision :: (Monad m) => CollisionAction m
despawnCollision _ = Aztecs.despawn

teleportCollision :: (Monad m) => Space -> CollisionAction m
teleportCollision target _ collided = do
    position <- Aztecs.lookup collided
    forM_ position $ \(Position _) -> Aztecs.insert collided $ Aztecs.bundle $ Position target

collisionSystem :: forall m. (Monad m, Typeable m) => Aztecs.Access m ()
collisionSystem = do
    snakes <- Aztecs.system $ Aztecs.runQueryFiltered ((,) <$> Aztecs.entity <*> Aztecs.query @_ @Position) $ Aztecs.with @m @SnakeHead
    Vector.forM_ snakes $ \(snakeEntityId, position) -> do
        collisions <- Aztecs.system $ Aztecs.runQuery (findCollisions position)
        Vector.forM_ collisions $ \(entityId, NewCollision action) -> do
            action entityId snakeEntityId

findCollisions :: forall m. (Monad m, Typeable m) => Position -> Aztecs.Query m (Aztecs.EntityID, NewCollision m)
findCollisions pos = fmap fst $ Aztecs.queryFilter ((==) pos . snd) $ (,) <$> ((,) <$> Aztecs.entity <*> Aztecs.query @m @(NewCollision m)) <*> Aztecs.query @m @Position

type Ticks = Integer

newtype Scores = Scores Integer

newtype Lifetime = Lifetime Ticks
    deriving stock (Show)
    deriving anyclass (Aztecs.Component m)

newtype SnakeDirection = SnakeDirection Space
    deriving newtype (Eq, Ord, Show)

data PlayerControls
    = ChangeDirection SnakeDirection
    | Special
    deriving stock (Eq, Ord, Show)

newtype ControlType = ControlType PlayerControls
    deriving stock (Show)

instance Eq ControlType where
    (ControlType (ChangeDirection _)) == ControlType ((ChangeDirection _)) = True
    (ControlType a) == (ControlType b) = a == b

instance Ord ControlType where
    compare a@(ControlType x) b@(ControlType y)
        | a == b = EQ
        | otherwise = compare x y

newtype PlayerInput = PlayerInput (PlayerId, PlayerControls)
    deriving newtype (Eq, Ord)

newtype InputQueue = InputQueue (Set.Set (PlayerId, ControlType))

initInputQueue :: InputQueue
initInputQueue = InputQueue mempty

addInput :: InputQueue -> PlayerInput -> InputQueue
addInput (InputQueue inputs) (PlayerInput (pId, control)) = InputQueue $ Set.insert (pId, ControlType control) inputs

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
        <> Aztecs.bundle (Renderable (Snake playerId))
  where
    startPos =
        case playerId of
            One -> Position (V2 1 1)
            Two -> Position (V2 19 19)

snakeSegment :: forall m. (Monad m, Typeable m) => PlayerId -> Aztecs.EntityID -> Space -> Ticks -> Aztecs.BundleT m
snakeSegment playerId snakeHeadId pos lifetime =
    Aztecs.bundle (Position pos)
        <> Aztecs.bundle (Renderable (SnakeSegment playerId))
        <> Aztecs.bundle (NewCollision @m despawnCollision)
        <> Aztecs.bundle (Lifetime lifetime)
        <> Aztecs.bundle (Aztecs.Parent snakeHeadId)

apple :: forall m. (Monad m, Typeable m) => Space -> Aztecs.BundleT m
apple pos =
    Aztecs.bundle (Position pos)
        <> Aztecs.bundle (Renderable Apple)
        <> Aztecs.bundle (NewCollision @m (appleCollision (Scores 1)))

wall :: forall m. (Monad m, Typeable m) => Space -> Aztecs.Access m ()
wall pos = void $ Aztecs.spawn $ Aztecs.bundle (Position pos) <> Aztecs.bundle (NewCollision @m despawnCollision) <> Aztecs.bundle (Renderable Wall)

sampleWorld :: forall m. (Monad m, Typeable m) => NonEmpty PlayerId -> WorldSettings -> Aztecs.Access m ()
sampleWorld players WorldSettings{width, height} = do
    forM_ players $ Aztecs.spawn . snakeHead
    void $ Aztecs.spawn $ apple (V2 1 5)
    void $ Aztecs.spawn $ Aztecs.bundle (Position (V2 10 10)) <> Aztecs.bundle (NewCollision @m (teleportCollision (V2 2 2))) <> Aztecs.bundle (Renderable Portal)
    void $ Aztecs.spawn $ Aztecs.bundle (Position (V2 10 19)) <> Aztecs.bundle (NewCollision @m (appleCollision (Scores (-5)))) <> Aztecs.bundle (Renderable Poison)
    void $ Aztecs.spawn $ Aztecs.bundle (World (V2 (toInteger height) (toInteger width)) (Seed 1) 0)
    forM_ walls $ \(x, y) -> wall $ V2 x y
    forM_ borders $ \(h, w) ->
        let (exitH, exitW) =
                case (h, w) of
                    (-1, y) -> (heightInt - 1, y)
                    (x, -1) -> (x, widthInt - 1)
                    (x, y) | x == heightInt -> (0, y)
                    (x, _) -> (x, 0)
         in void $ Aztecs.spawn $ Aztecs.bundle (Position (V2 h w)) <> Aztecs.bundle (NewCollision @m $ teleportCollision (V2 exitH exitW))
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

gameStep :: (Monad m, Typeable m) => InputQueue -> Aztecs.Access m Bool
gameStep playerInput = do
    timeSystem
    lifetimeSystem
    playerActionSystem playerInput
    snakeGhostSystem
    moveSystem
    collisionSystem
    endGameSystem

timeSystem :: (Monad m) => Aztecs.Access m ()
timeSystem = void $ Aztecs.system $ Aztecs.runQuery $ Aztecs.queryMap (\w@World{tick} -> w{tick = tick + 1})

moveSystem :: (Monad m) => Aztecs.Access m ()
moveSystem = void $ Aztecs.system $ Aztecs.runQuery $ Aztecs.queryMapWith move Aztecs.query
  where
    move (Velocity v) (Position pos) = Position $ pos + v

snakeGhostSystem :: (Monad m, Typeable m) => Aztecs.Access m ()
snakeGhostSystem = do
    snakes <- Aztecs.system $ Aztecs.runQuery $ Aztecs.queryFilter (\(_, _, _, Velocity v) -> v /= V2 0 0) $ (,,,) <$> Aztecs.entity <*> Aztecs.query <*> Aztecs.query <*> Aztecs.query
    Vector.forM_ snakes $ \(snakeEntityId, SnakeHead pId l, Position pos, _) -> Aztecs.spawn_ $ snakeSegment pId snakeEntityId pos $ toInteger l

lifetimeSystem :: (Monad m) => Aztecs.Access m ()
lifetimeSystem = do
    dead <- fmap (Vector.filter (isDead . snd)) $ Aztecs.system $ Aztecs.runQuery $ (,) <$> Aztecs.entity <*> Aztecs.queryMap (\(Lifetime x) -> Lifetime $ max 0 $ x - 1)
    Vector.forM_ dead $ \(entityId, _) -> Aztecs.despawn entityId

endGameSystem :: (Monad m) => Aztecs.Access m Bool
endGameSystem = fmap Vector.null $ Aztecs.system $ Aztecs.runQuery $ Aztecs.query @_ @SnakeHead

playerActionSystem :: (Monad m, Typeable m) => InputQueue -> Aztecs.Access m ()
playerActionSystem (InputQueue input) = mapM_ playerAction $ Set.map (\(pId, ControlType control) -> PlayerInput (pId, control)) input

step :: (Monad m, Typeable m) => InputQueue -> State m -> m (Bool, State m)
step playerInput (State world) = second State <$> Aztecs.runAccess (gameStep playerInput) world

playerAction :: (Monad m, Typeable m) => PlayerInput -> Aztecs.Access m ()
playerAction (PlayerInput (playerId, ChangeDirection (SnakeDirection dir))) =
    void $ Aztecs.system $ Aztecs.runQuery $ Aztecs.queryFilter (\(SnakeHead pId _, _) -> pId == playerId) ((,) <$> Aztecs.query @_ @SnakeHead <*> Aztecs.queryMap mapVel)
  where
    mapVel (Velocity currentVel)
        | currentVel + dir == V2 0 0 = Velocity currentVel
        | otherwise = Velocity dir
playerAction (PlayerInput (pId, Special)) = do
    World{size = V2 width height} <- Aztecs.system $ Aztecs.runQuerySingle Aztecs.query
    snakes <- Aztecs.system $ Aztecs.runQuery $ Aztecs.queryFilter (\(SnakeHead{playerId}, _, _) -> playerId == pId) ((,,) <$> Aztecs.query @_ @SnakeHead <*> Aztecs.query @_ @Position <*> Aztecs.query @_ @Velocity)
    forM_ snakes $ \(_, Position (V2 px py), Velocity (V2 vx vy)) ->
        forM_ (Vector.generate (fromInteger $ max width height) (\x -> V2 (max 0 (min (width - 1) (px + toInteger x * vx))) (max 0 (min (height - 1) (py + toInteger x * vy))))) $ \laserPos -> do
            Aztecs.spawn_ $
                Aztecs.bundle (Position laserPos)
                    <> Aztecs.bundle (Renderable Laser)
                    <> Aztecs.bundle (Lifetime 1)
            collisions <- Aztecs.system $ Aztecs.runQuery (findCollisions $ Position laserPos)
            Vector.forM_ collisions $ Aztecs.despawn . fst

render :: forall m. (Monad m) => State m -> m Render
render (State world) = fst <$> Aztecs.runAccess doRender world

doRender :: forall m. (Monad m) => Aztecs.Access m Render
doRender = do
    World{size = V2 width height} <- Aztecs.system $ Aztecs.runQuerySingle $ Aztecs.query @m @World
    positions <- Aztecs.system $ Aztecs.runQuery $ (,) <$> Aztecs.query <*> Aztecs.query
    let emptyGrid = Vector.replicate (fromInteger width) $ Vector.replicate (fromInteger height) Empty
        fullGrid = Vector.foldr writeElem emptyGrid positions
    pure $ Render fullGrid

writeElem :: (Position, Renderable) -> Vector.Vector (Vector.Vector Glyph) -> Vector.Vector (Vector.Vector Glyph)
writeElem (Position (V2 x y), Renderable glyph) grid =
    let row = grid Vector.! fromInteger x
        newRow = row Vector.// [(fromInteger y, glyph)]
     in grid Vector.// [(fromInteger x, newRow)]
