{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Mambda 
    where

import Mambda.Snake

data Game a d = Game 
    { snake :: Snake a
    , snakeDirection :: d
    , worldGeometry :: Geometry a d
    , pause :: Bool
    }

newtype Geometry a d = Geometry { moveDir :: d -> a -> a }

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

class Monad m => GameMonad m a d | d -> a where
    getCommands :: m [GameCommand d]
    renderGame :: Game a d -> m ()

startGame 
    :: GameMonad m a d
    => Geometry a d 
    -> d
    -> a
    -> m (Game a d)
startGame geometry initDir snakeInit =
    gameLoop newGame
  where
    snake = createSnake snakeInit
    newGame = Game snake initDir geometry False

gameLoop 
    :: GameMonad m a d 
    => Game a d 
    -> m (Game a d)
gameLoop game = 
    fmap (step . foldState) getCommands 
    >>= \newState -> do
        renderGame newState
        gameLoop newState
  where
    foldState = foldl processCommand game
