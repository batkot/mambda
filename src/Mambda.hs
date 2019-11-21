{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Mambda 
    ( module Snake
    , module Rules
    , module Utils
    , module Flatland
    , GameMonad(..)
    , startGame
    , gameLoop
    )
    where

import Mambda.Snake as Snake
import Mambda.Rules as Rules
import Mambda.Utils as Utils
import Mambda.Flatland as Flatland

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
