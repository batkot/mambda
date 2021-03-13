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

data Object a = Object
    { location :: a
    , collision :: Game a -> Game a
    } 

instance Show a => Show (Object a) where
    show (Object a _) = show a

instance Eq a => Eq (Object a) where
    (Object a _)  == (Object b _) = a == b

food :: Eq a => PositiveInt -> a -> [a] -> Object a
food grow loc next = Object loc $ 
    \g -> 
        case next of
            [] -> g { status = Finished }
            (x:xs) -> 
                let newFood = food grow x xs
                    objs = filter ((/=) loc . location) . objects $ g
                in g 
                    { snake = increaseGrow grow . snake $ g 
                    , objects = newFood : objs
                    , score = (+1) . score $ g
                    }

wall :: Eq a => a -> Object a
wall loc = Object loc finishGame

teleport :: Eq a => a -> a -> Object a
teleport loc destination = Object loc $ 
    \g -> g 
    { snake = move destination . snake $ g
    }

snakeToObjects :: Snake a -> [Object a]
snakeToObjects = fmap makeCollisionObject . snakeBody
  where
    snakeBody = NE.tail . body 
    makeCollisionObject loc = Object loc finishGame

finishGame :: Game a -> Game a
finishGame g = g { status = Finished }

data GameCommand a 
    = ChangeSpeed a
    | TogglePause
    deriving (Show,Eq, Functor)

processCommand :: Game a -> GameCommand a -> Game a
processCommand game (ChangeSpeed a) = game { snakeSpeed = a }
processCommand game@(Game _ _ _ _ status) TogglePause = game { status = toggled status }
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
