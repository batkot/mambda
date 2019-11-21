module Mambda.Rules
    where

import Mambda.Snake

data Game a d = Game 
    { snake :: Snake a
    , snakeDirection :: d
    , worldGeometry :: Geometry a d
    , pause :: Bool
    } deriving (Show, Eq)

newtype Geometry a d = Geometry { moveDir :: d -> a -> a } 

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

step :: Game a d -> Game a d
step game@(Game _ _ _ True) = game
step game@(Game snake dir geometry False) = 
    game { snake = newSnake } 
  where
    moved = moveDir geometry dir $ getHead snake
    newSnake = move moved snake
