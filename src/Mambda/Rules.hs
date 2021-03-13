module Mambda.Rules
    where

import Mambda.Snake
import Mambda.Utils

import Data.List (filter)
import Data.List.NonEmpty as NE (tail)

data Game a = Game 
    { snake :: Snake a
    , snakeSpeed :: a
    , objects :: [Object a]
    , score :: Int
    , pause :: Bool
    , finished :: Bool
    } deriving (Show, Eq)

data Object a = Object
    { location :: a
    , collision :: Game a -> Game a
    }

instance Show a => Show (Object a) where
    show (Object a _) = show a

instance Eq a => Eq (Object a) where
    (Object a _)  == (Object b _) = a == b

food :: Eq a => PositiveInt -> a -> Object a
food grow loc = Object loc $ 
    \g -> g 
    { snake = increaseGrow grow . snake $ g 
    , objects = filter ((/=) loc . location) . objects $ g
    , score = (+1) . score $ g
    }

snakeToObjects :: Snake a -> [Object a]
snakeToObjects = fmap makeCollisionObject . snakeBody
  where
    snakeBody = NE.tail . body 
    makeCollisionObject loc = Object loc $ \g -> g { finished = True }

data GameCommand a 
    = ChangeSpeed a
    | TogglePause
    deriving (Show,Eq)

processCommand :: Game a -> GameCommand a -> Game a
processCommand game (ChangeSpeed a) = game { snakeSpeed = a }
processCommand game@Game{ pause = pause } TogglePause = game { pause = not pause }

step :: (Eq a, Monoid a) => Game a -> Game a
step game@(Game _ _ _ _ _ True) = game
step game@(Game _ _ _ _ True _) = game
step game@(Game s speed objects _ False _) = 
    newGame
  where
    moved = getHead s <> speed
    movedGame = game { snake = move moved s }
    snakeBody = snakeToObjects $ snake movedGame
    newGame = foldr collision movedGame $ filter ((==) moved . location) $ snakeBody ++ objects
