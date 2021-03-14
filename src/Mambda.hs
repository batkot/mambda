{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}

module Mambda 
    ( module Snake
    , module Rules
    , module Utils
    , module Objects

    , GameMonad(..)
    , startGame
    , gameLoop
    , GameCommand(..)
    )
    where

import Mambda.Snake as Snake
import Mambda.Rules as Rules
import Mambda.Utils as Utils
import Mambda.Objects as Objects

class Monad m => GameMonad m a where
    getCommands :: m [GameCommand a]
    renderGame :: Game a -> m ()

startGame 
    :: GameMonad m a
    => Eq a
    => Monoid a
    => [Object a]
    -> a
    -> a
    -> m [a]
    -> m (Game a)
startGame objects initDir snakeInit foodSource = do
    f <- foodSource
    let obj = case f of 
                [] -> objects
                (x:xs) -> food one x xs : objects
    gameLoop $ newGame obj
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
    foldState = foldr processCommand game

data GameCommand a 
    = ChangeSpeed a
    | TogglePause
    deriving (Show,Eq, Functor)

processCommand :: GameCommand a -> Game a -> Game a
processCommand (ChangeSpeed a) game = game { snakeSpeed = a }
processCommand TogglePause game@Game{ status = status } = game { status = toggled status }
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
