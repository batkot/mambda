module Mambda 
    where

import Mambda.Snake

data Game a d = Game 
    { snake :: Snake a
    , snakeDirection :: d
    , worldTopology :: Topology a d
    }

data Topology a d = Topology 
    { moveDir :: d -> a -> a
    }

data GameCommand a
    = ChangeDirection a
    | Tick

updateState :: Game a d -> GameCommand d -> Game a d
updateState game (ChangeDirection d) = game { snakeDirection = d }
updateState (Game snake dir top) Tick = 
    Game newSnake dir top
  where
    moved = moveDir top dir $ getHead snake
    newSnake = move moved snake

