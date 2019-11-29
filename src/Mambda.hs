{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    randomObject :: m (Object a d)

startGame 
    :: GameMonad m a d
    => Eq a
    => Geometry a d 
    -> d
    -> a
    -> m (Game a d)
startGame geometry initDir snakeInit =
    gameLoop newGame
  where
    snake = createSnake snakeInit
    newGame = Game snake initDir geometry [] 0 False

gameLoop 
    :: GameMonad m a d 
    => Eq a
    => Game a d 
    -> m (Game a d)
gameLoop game = 
    fmap (step . foldState) getCommands 
    >>= \s -> do
        renderGame s
        newState <- case objects s of 
            [] -> do 
                obj <- randomObject
                return s { objects = [obj] }
            _ -> return s
        gameLoop newState
  where
    foldState = foldl processCommand game
