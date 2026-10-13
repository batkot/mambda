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
  Seed (..),
) where

import Prelude hiding (init, (!!))

import Aztecs qualified
import Aztecs.ECS.World qualified as World
import Control.Monad.Extra
import Data.Bifunctor
import Data.Foldable (traverse_)
import Data.List.NonEmpty hiding (init)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import Data.Vector qualified as Vector
import Data.Word (Word8)
import Linear
import Mambda.Movement qualified as Movement
import Mambda.Time qualified as Time
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
  | LaserBeam
  | Laser
  deriving stock (Show)

type Space = V2 Integer

data PlayerId = One | Two
  deriving stock (Show, Eq, Ord)

newtype Seed = Seed Integer
  deriving (Show)

mkSeed :: Seed -> Time.Clock -> Seed
mkSeed (Seed x) clock = Seed $ x + Time.ticks clock

data World = World
  { size :: Space
  , seed :: Seed
  , clock :: Time.Clock
  }
  deriving anyclass (Aztecs.Component m)

data Item = Item
  deriving stock (Show)

data SnakeHead = SnakeHead
  { playerId :: PlayerId
  , length :: Word8
  , special :: Maybe Item
  }
  deriving stock (Show)
  deriving anyclass (Aztecs.Component m)

newtype Renderable = Renderable Glyph
  deriving stock (Show)
  deriving anyclass (Aztecs.Component m)

newtype Collidable m = Collidable (CollisionAction m)
  deriving anyclass (Aztecs.Component m)

type CollisionAction m = Aztecs.EntityID -> Aztecs.EntityID -> Aztecs.Access m ()

newtype Special m = Special' (Aztecs.EntityID -> Aztecs.Access m ())
  deriving anyclass (Aztecs.Component m)

item :: forall m. (Monad m, Typeable m) => Special m -> Aztecs.BundleT m
item special =
  Aztecs.bundle $ Collidable @m $ \self other -> do
    headMb <- Aztecs.lookup @m @SnakeHead other
    forM_ headMb $ const $ Aztecs.insert other $ Aztecs.bundle special
    Aztecs.despawn self

score :: (Monad m) => Aztecs.EntityID -> Scores -> Aztecs.Access m ()
score entity (Scores score') = do
  headMaybe <- Aztecs.lookup @_ @SnakeHead entity
  forM_ headMaybe $ \(SnakeHead player score item) ->
    Aztecs.insertUntracked entity $ Aztecs.bundle $ SnakeHead player (fromInteger $ max 1 $ toInteger score + score') item
  snakeSegments <- maybe mempty Aztecs.unChildren <$> Aztecs.lookup entity
  let bumpLifetime entityId = do
        currentLifetime <- maybe 0 (\(SnakeSegment' x) -> x) <$> Aztecs.lookup entityId
        Aztecs.insertUntracked entityId $ Aztecs.bundle $ SnakeSegment' $ max 1 $ currentLifetime + fromInteger score'
  traverse_ bumpLifetime snakeSegments

spawnEntity :: forall m. (Monad m) => (Space -> Aztecs.BundleT m) -> Aztecs.Access m ()
spawnEntity toSpawn = do
  takenSpots <- Aztecs.system $ Aztecs.runQuery $ Aztecs.query @m @Movement.Positioned
  World{size = V2 width height, seed, clock} <- Aztecs.system $ Aztecs.runQuerySingle $ Aztecs.query @m @World
  let free = NonEmpty.nonEmpty [Movement.Positioned (V2 w h) | w <- [0 .. width - 1], h <- [0 .. height - 1], Vector.notElem (Movement.Positioned (V2 w h)) takenSpots]
  case free of
    Nothing -> pure ()
    Just available -> Aztecs.spawn_ $ toSpawn $ (\(Movement.Positioned x) -> x) $ pick (mkSeed seed clock) available

pick :: Seed -> NonEmpty a -> a
pick (Seed seed) elements = elements !! index
 where
  index = fst $ Random.uniformR (0, NonEmpty.length elements - 1) stdGen
  stdGen = Random.mkStdGen $ fromInteger seed

appleCollision :: (Monad m, Typeable m) => Scores -> CollisionAction m
appleCollision scores appleEntityId snakeEntityId = do
  score snakeEntityId scores
  void $ Aztecs.despawn appleEntityId
  World{clock} <- Aztecs.system $ Aztecs.runQuerySingle Aztecs.query
  spawnEntity apple

despawnCollision :: (Monad m) => CollisionAction m
despawnCollision _ = Aztecs.despawn

teleportCollision :: (Monad m) => Space -> CollisionAction m
teleportCollision target _ collided = do
  position <- Aztecs.lookup @_ @Movement.Positioned collided
  forM_ position $ \_ -> Aztecs.insert collided $ Movement.positioned target

collisionSystem :: forall m. (Monad m, Typeable m) => Aztecs.Access m ()
collisionSystem = do
  snakes <- Aztecs.system $ Aztecs.runQueryFiltered ((,) <$> Aztecs.entity <*> Aztecs.query @_ @Movement.Positioned) $ Aztecs.with @m @SnakeHead
  Vector.forM_ snakes collisionLoop
 where
  collisionLoop (snakeEntityId, position) = do
    collisions <- Aztecs.system $ Aztecs.runQuery (findCollisions position)
    Vector.forM_ collisions $ \(entityId, Collidable action) -> do
      action entityId snakeEntityId
      currentPos <- Aztecs.lookup snakeEntityId
      whenJust currentPos $ \newPos ->
        unless (newPos == position) $ collisionLoop (snakeEntityId, newPos)

findCollisions :: forall m. (Monad m, Typeable m) => Movement.Positioned -> Aztecs.Query m (Aztecs.EntityID, Collidable m)
findCollisions pos = fmap fst $ Aztecs.queryFilter ((==) pos . snd) $ (,) <$> ((,) <$> Aztecs.entity <*> Aztecs.query @m @(Collidable m)) <*> Aztecs.query @m @Movement.Positioned

newtype Scores = Scores Integer

newtype Lifetime = Lifetime Time.Time
  deriving anyclass (Aztecs.Component m)

newtype SnakeSegment = SnakeSegment' Integer
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

data WorldSettings = WorldSettings
  { width :: Word8
  , height :: Word8
  }

snakeHead :: (Monad m) => Time.Clock -> PlayerId -> Aztecs.BundleT m
snakeHead clock playerId =
  Aztecs.bundle (SnakeHead playerId 3 Nothing)
    <> Movement.moving clock startPos (Movement.Velocity Movement.down (Time.mkTime startSpeed))
    <> Aztecs.bundle (Renderable (Snake playerId))
 where
  startPos =
    case playerId of
      One -> V2 1 1
      Two -> V2 19 19
  startSpeed =
    case playerId of
      One -> 16
      Two -> 8


snakeSegment :: forall m. (Monad m, Typeable m) => PlayerId -> Aztecs.EntityID -> Space -> Integer -> Aztecs.BundleT m
snakeSegment playerId snakeHeadId pos score =
  Movement.positioned pos
    <> Aztecs.bundle (Renderable (SnakeSegment playerId))
    <> Aztecs.bundle (Collidable @m despawnCollision)
    <> Aztecs.bundle (SnakeSegment' score)
    <> Aztecs.bundle (Aztecs.Parent snakeHeadId)

apple :: forall m. (Monad m, Typeable m) => Space -> Aztecs.BundleT m
apple pos =
  Movement.positioned pos
    <> Aztecs.bundle (Renderable Apple)
    <> Aztecs.bundle (Collidable @m (appleCollision (Scores 1)))

goldenApple :: forall m. (Monad m, Typeable m) => Space -> Aztecs.BundleT m
goldenApple pos =
  Movement.positioned pos
    <> Aztecs.bundle (Renderable GoldenApple)
    <> Aztecs.bundle (Collidable @m (appleCollision (Scores 5)))

wall :: forall m. (Monad m, Typeable m) => Space -> Aztecs.Access m ()
wall pos = void $ Aztecs.spawn $ Movement.positioned pos <> Aztecs.bundle (Collidable @m despawnCollision) <> Aztecs.bundle (Renderable Wall)

initWorld :: forall m. (Monad m, Typeable m) => Time.Clock -> NonEmpty PlayerId -> Seed -> WorldSettings -> Aztecs.Access m ()
initWorld clock players seed WorldSettings{width, height} = do
  void $ Aztecs.spawn $ Aztecs.bundle (World (V2 (toInteger height) (toInteger width)) seed clock)
  forM_ players $ Aztecs.spawn . snakeHead clock
  Aztecs.spawn_ $ Movement.positioned (V2 10 10) <> bombItem <> Aztecs.bundle (Renderable Laser)
  -- void $ Aztecs.spawn $ Aztecs.bundle (Position (V2 10 10)) <> Aztecs.bundle (Collidable @m (teleportCollision (V2 2 2))) <> Aztecs.bundle (Renderable Portal)
  -- void $ Aztecs.spawn $ Aztecs.bundle (Position (V2 10 19)) <> Aztecs.bundle (Lifetime 5) <> Aztecs.bundle (Renderable Poison)
  forM_ walls $ \(x, y) -> wall $ V2 x y
  forM_ borders $ \(h, w) ->
    let (exitH, exitW) =
          case (h, w) of
            (-1, y) -> (heightInt - 1, y)
            (x, -1) -> (x, widthInt - 1)
            (x, y) | x == heightInt -> (0, y)
            (x, _) -> (x, 0)
     in void $ Aztecs.spawn $ Movement.positioned (V2 h w) <> Aztecs.bundle (Collidable @m $ teleportCollision (V2 exitH exitW))
  spawnEntity apple
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

init :: (Monad m, Typeable m) => Time.Clock -> NonEmpty PlayerId -> Seed -> WorldSettings -> m (State m)
init clock players seed worldSettings = State . snd <$> Aztecs.runAccess (initWorld clock players seed worldSettings) World.empty

gameStep :: (Monad m, Typeable m) => Time.Time -> InputQueue -> Aztecs.Access m Bool
gameStep dt playerInput = do
  clock <- timeSystem dt
  playerActionSystem playerInput
  snakeGhostSystem clock
  Movement.moveSystem clock
  collisionSystem
  endGameSystem

timeSystem :: (Monad m) => Time.Time -> Aztecs.Access m Time.Clock
timeSystem dt = do
  World{clock} <- Aztecs.system $ Aztecs.runQuerySingle $ Aztecs.queryMap (\w@World{clock} -> w{clock = Time.tick dt clock})
  dead <- Aztecs.system $ Aztecs.runQuery $ Aztecs.queryFilter (\(_, Lifetime x) -> x <= Time.time clock) ((,) <$> Aztecs.entity <*> Aztecs.query)
  Vector.forM_ dead $ \(entityId, _) -> Aztecs.despawn entityId
  pure clock

snakeGhostSystem :: (Monad m, Typeable m) => Time.Clock -> Aztecs.Access m ()
snakeGhostSystem clock = do
  snakes <- Aztecs.system $ Aztecs.runQuery $ Aztecs.queryFilter (\(_, _, _, m@Movement.Moving{}) -> Movement.shouldMove clock m) $ (,,,) <$> Aztecs.entity <*> Aztecs.query <*> Aztecs.query <*> Aztecs.query
  Vector.forM_ snakes $ \(snakeEntityId, SnakeHead pId l _, Movement.Positioned pos, _) -> do
    Aztecs.spawn_ $ snakeSegment pId snakeEntityId pos $ toInteger l
    segments <- fmap (maybe mempty Aztecs.unChildren) (Aztecs.lookup snakeEntityId)
    x <- Aztecs.system $ Aztecs.runQuery $ Aztecs.queryFilter (\(segmentId, _) -> Set.member segmentId segments) $ (,) <$> Aztecs.entity <*> Aztecs.queryMap (\(SnakeSegment' x) -> SnakeSegment' $ x - 1)
    forM_ x $ \(entityId, SnakeSegment' life) -> when (life <= 0) $ Aztecs.despawn entityId

endGameSystem :: (Monad m) => Aztecs.Access m Bool
endGameSystem = fmap Vector.null $ Aztecs.system $ Aztecs.runQuery $ Aztecs.query @_ @SnakeHead

playerActionSystem :: (Monad m, Typeable m) => InputQueue -> Aztecs.Access m ()
playerActionSystem (InputQueue input) = mapM_ playerAction $ Set.map (\(pId, ControlType control) -> PlayerInput (pId, control)) input

step :: (Monad m, Typeable m) => Time.Time -> InputQueue -> State m -> m (Bool, State m)
step dt playerInput (State world) = second State <$> Aztecs.runAccess (gameStep dt playerInput) world

playerAction :: forall m. (Monad m, Typeable m) => PlayerInput -> Aztecs.Access m ()
playerAction (PlayerInput (playerId, ChangeDirection (SnakeDirection dir))) =
  void $ Aztecs.system $ Aztecs.runQuery $ Aztecs.queryFilter (\(SnakeHead pId _ _, _) -> pId == playerId) ((,) <$> Aztecs.query @_ @SnakeHead <*> Aztecs.queryMap foo)
 where
  foo :: Movement.Moving -> Movement.Moving
  foo m@Movement.Moving{velocity} = m{Movement.velocity = mapVel velocity}
  mapVel vel@(Movement.Velocity (Movement.Direction currentDir) _)
    | currentDir + dir == V2 0 0 = vel
    | otherwise = vel{Movement.direction = Movement.Direction dir}
playerAction (PlayerInput (pId, Special)) = do
  snakes <- fmap (fmap fst) $ Aztecs.system $ Aztecs.runQuery $ Aztecs.queryFilter (\(_, SnakeHead{playerId}) -> playerId == pId) ((,) <$> Aztecs.entity <*> Aztecs.query)
  forM_ snakes $ \snakeEntityId -> do
    special <- Aztecs.lookup snakeEntityId
    forM_ special $ \(Special' specialAction) -> do
      specialAction snakeEntityId
      -- Component removal is broken in Aztecs
      Aztecs.insert snakeEntityId $ Aztecs.bundle $ emptySpecial @m

laserItem :: forall m. (Monad m, Typeable m) => Aztecs.BundleT m
laserItem =
  item . Special' @m $ \snakeEntityId -> do
    (posMb, velMb) <- (,) <$> Aztecs.lookup snakeEntityId <*> Aztecs.lookup snakeEntityId
    fromMaybe (pure ()) $ fireLaser <$> posMb <*> fmap (\(Movement.Moving v _) -> v) velMb

fireLaser :: (Monad m, Typeable m) => Movement.Positioned -> Movement.Velocity -> Aztecs.Access m ()
fireLaser (Movement.Positioned pos) (Movement.Velocity (Movement.Direction dir) _) = do
  World{size = V2 width height, clock} <- Aztecs.system $ Aztecs.runQuerySingle Aztecs.query
  let inWorldBounds (V2 x y) = 0 <= x && x < width && 0 <= y && y < height
      laserBeam = NonEmpty.takeWhile inWorldBounds $ NonEmpty.iterate (+ dir) pos
  forM_ laserBeam $ \laserPos -> do
    Aztecs.spawn_ $
      Aztecs.bundle (Movement.Positioned laserPos)
        <> Aztecs.bundle (Renderable LaserBeam)
        <> Aztecs.bundle (Lifetime $ Time.time clock)
    collisions <- Aztecs.system $ Aztecs.runQuery (findCollisions $ Movement.Positioned laserPos)
    Vector.forM_ collisions $ Aztecs.despawn . fst

bombItem :: forall m. (Monad m, Typeable m) => Aztecs.BundleT m
bombItem =
  item . Special' @m $ \snakeEntityId -> do
    (lastSegmentPosition, _, _) <- Aztecs.system $ Aztecs.runQuerySingle $ Aztecs.queryFilter (\(Movement.Positioned _, SnakeSegment' x, Aztecs.Parent parentId) -> x == 1 && parentId == snakeEntityId) ((,,) <$> Aztecs.query <*> Aztecs.query <*> Aztecs.query)
    Aztecs.spawn_ $ laserItem <> Aztecs.bundle lastSegmentPosition <> Aztecs.bundle (Renderable Laser)

emptySpecial :: forall m. (Monad m) => Special m
emptySpecial = Special' @m $ const $ pure ()

render :: forall m. (Monad m) => State m -> m Render
render (State world) = fst <$> Aztecs.runAccess doRender world

doRender :: forall m. (Monad m) => Aztecs.Access m Render
doRender = do
  World{size = V2 width height} <- Aztecs.system $ Aztecs.runQuerySingle $ Aztecs.query @m @World
  positions <- Aztecs.system $ Aztecs.runQuery $ (,) <$> Aztecs.query <*> Aztecs.query
  let emptyGrid = Vector.replicate (fromInteger width) $ Vector.replicate (fromInteger height) Empty
      fullGrid = Vector.foldr writeElem emptyGrid positions
  pure $ Render fullGrid

writeElem :: (Movement.Positioned, Renderable) -> Vector.Vector (Vector.Vector Glyph) -> Vector.Vector (Vector.Vector Glyph)
writeElem (Movement.Positioned (V2 x y), Renderable glyph) grid =
  let row = grid Vector.! fromInteger x
      newRow = row Vector.// [(fromInteger y, glyph)]
   in grid Vector.// [(fromInteger x, newRow)]
