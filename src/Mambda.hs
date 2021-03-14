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
    gameLoop $ Rules.newGame snake initDir obj
  where
    snake = createSnake snakeInit

gameLoop 
    :: GameMonad m a
    => Eq a
    => Monoid a
    => Game a
    -> m (Game a)
gameLoop game = do
    newState <- step . foldState <$> getCommands 
    renderGame newState
    case gameStatus newState of 
        Finished -> return newState
        _ -> gameLoop newState
  where
    foldState = foldr processCommand game

data GameCommand a 
    = ChangeSpeed a
    | TogglePause
    | Quit
    deriving (Show,Eq, Functor)

processCommand :: (Eq a, Monoid a) => GameCommand a -> Game a -> Game a
processCommand (ChangeSpeed a) game = changeSnakeSpeed a game
processCommand Quit game = finishGame game
processCommand TogglePause game = 
    case gameStatus game of
        Paused -> resumeGame game
        Running -> pauseGame game
        Finished -> game

step :: (Eq a, Monoid a) => Game a -> Game a
step game = 
    foldr collision movedGame $ filter ((==) snakeHead . location) $ gameObjects movedGame
  where
    movedGame = stepSnakeEffect game
    snakeHead = getHead . gameSnake $ movedGame
