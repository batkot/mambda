module Mambda.Rules
    where

import Mambda.Snake
import Mambda.Utils

import Data.List (filter)

data Game a d = Game 
    { snake :: Snake a
    , snakeDirection :: d
    , worldGeometry :: Geometry a d
    , objects :: [Object a d]
    , score :: Int
    , pause :: Bool
    } deriving (Show, Eq)

newtype Geometry a d = Geometry { moveDir :: d -> a -> a } 

data Object a d = Object
    { location :: a
    , collision :: Game a d -> Game a d
    }

instance Show a => Show (Object a d) where
    show (Object a _) = show a

instance Eq a => Eq (Object a d) where
    (Object a _)  == (Object b _) = a == b

food :: Eq a => PositiveInt -> a -> Object a d
food grow loc = Object loc $ 
    \g -> g 
    { snake = (increaseGrow grow . snake) g 
    , objects = filter ((/=) loc . location) $ objects g
    , score = (+1) $ score g
    }

instance Show (Geometry a d) where
    show = const "Geometry"

instance Eq (Geometry a d) where
    (==) _ _ = True

data GameCommand a 
    = ChangeDirection a 
    | TogglePause
    deriving (Show,Eq)

processCommand :: Game a d -> GameCommand d -> Game a d
processCommand game (ChangeDirection d) = game { snakeDirection = d }
processCommand game@Game{ pause = pause } TogglePause = game { pause = not pause }

step :: Eq a => Game a d -> Game a d
step game@(Game _ _ _ _ _ True) = game
step game@(Game snake dir geometry objects _ False) = 
    newGame
  where
    moved = moveDir geometry dir $ getHead snake
    movedGame = game { snake = move moved snake }
    newGame = foldr collision movedGame $ filter ((==) moved . location) objects
