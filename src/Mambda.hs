{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    => a
    -> a
    -> m (Game a)
startGame initDir snakeInit =
    gameLoop newGame
  where
    snake = createSnake snakeInit
    newGame = Game snake initDir [] 0 False False

gameLoop 
    :: GameMonad m a
    => Eq a
    => Monoid a
    => Game a
    -> m (Game a)
gameLoop game = 
    fmap (step . foldState) getCommands 
    >>= \s -> do
        renderGame s
        newState <- case objects s of 
            [] -> do 
                obj <- randomObject
                return s { objects = [obj] }
            _ -> return s
        case finished newState of
            True -> return newState
            False -> gameLoop newState
  where
    foldState = foldl processCommand game
