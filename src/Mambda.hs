{-# LANGUAGE MultiParamTypeClasses #-}

module Mambda 
    ( module Snake
    , module Rules
    , module Utils
    , GameMonad(..)
    , startGame
    , gameLoop
    )
    where

import Mambda.Snake as Snake
import Mambda.Rules as Rules
import Mambda.Utils as Utils

class Monad m => GameMonad m a where
    getCommands :: m [GameCommand a]
    renderGame :: Game a -> m ()
    randomObject :: m (Object a)

startGame 
    :: GameMonad m a
    => Eq a
    => Monoid a
    => [Object a]
    -> a
    -> a
    -> m (Game a)
startGame objects initDir snakeInit = do
    startFood <- randomObject
    gameLoop $ newGame $ startFood : objects
  where
    snake = createSnake snakeInit
    newGame objects = Game snake initDir objects 0 Running

gameLoop 
    :: GameMonad m a
    => Eq a
    => Monoid a
    => Game a
    -> m (Game a)
gameLoop game = do
    newState <- step . foldState <$> getCommands 
    renderGame newState
    case status newState of 
        Finished -> return newState
        _ -> gameLoop newState
  where
    foldState = foldl processCommand game
