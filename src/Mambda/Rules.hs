{-# LANGUAGE DeriveFunctor #-}
module Mambda.Rules
    where

import Mambda.Snake
import Mambda.Utils

import Data.List (filter)
import Data.List.NonEmpty as NE (tail)

data GameStatus 
    = Running 
    | Paused 
    | Finished 
    deriving (Eq, Show)

data Game a = Game 
    { snake :: Snake a
    , snakeSpeed :: a
    , objects :: [Object a]
    , score :: Int
    , status :: GameStatus
    } deriving (Show, Eq)

type GameEffect a = Game a -> Game a

data Object a = Object
    { location :: a
    , collision :: GameEffect a
    } 

instance Show a => Show (Object a) where
    show (Object a _) = show a

instance Eq a => Eq (Object a) where
    (Object a _)  == (Object b _) = a == b

scoreGame :: GameEffect a
scoreGame g  = g { score = (+1) . score $ g }

snakeToObjects :: Snake a -> [Object a]
snakeToObjects = fmap makeCollisionObject . snakeBody
  where
    snakeBody = NE.tail . body 
    makeCollisionObject loc = Object loc finishGame

finishGame :: GameEffect a
finishGame g = g { status = Finished }

moveSnake :: Monoid a => a -> Snake a -> Snake a
moveSnake vel snake = 
    move (getHead snake <> vel) snake

-- Lens
modifySnake :: (Snake a -> Snake a) -> GameEffect a
modifySnake f g@Game{ snake = snake } = g { snake = f snake }

modifyObject :: (Object a -> Object a) -> GameEffect a
modifyObject f g@Game { objects = objects } = g { objects = f <$> objects }

growSnakeEffect :: PositiveInt -> GameEffect a
growSnakeEffect grow = modifySnake $ increaseGrow grow

moveSnakeEffect :: Monoid a => a -> GameEffect a
moveSnakeEffect vel = modifySnake $ moveSnake vel

replaceObject :: Eq a => a -> Object a -> GameEffect a
replaceObject loc obj =  modifyObject $ \o -> if location o == loc then obj else o

data GameCommand a 
    = ChangeSpeed a
    | TogglePause
    deriving (Show,Eq, Functor)

processCommand :: GameCommand a -> Game a -> Game a
processCommand (ChangeSpeed a) game = game { snakeSpeed = a }
processCommand TogglePause game@Game{ status = status } = game { status = toggled status }
    where
      toggled Running = Paused
      toggled Paused = Running
      toggled Finished = Finished

step :: (Eq a, Monoid a) => Game a -> Game a
step game@(Game s speed objects _ Running) = 
    newGame
  where
    moved = getHead s <> speed
    movedGame = game { snake = move moved s }
    snakeBody = snakeToObjects $ snake movedGame
    newGame = foldr collision movedGame $ filter ((==) moved . location) $ snakeBody ++ objects
step game = game
