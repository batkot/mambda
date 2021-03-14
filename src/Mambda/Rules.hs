module Mambda.Rules
    ( Game
    , newGame
    , gameObjects
    , gameStatus
    , gameSnake
    , gameScore

    , GameStatus(..)
    , Object(..)

    , GameEffect
    , changeSnakeSpeed
    , scoreGame
    , finishGame
    , pauseGame
    , resumeGame
    , replaceObject
    , growSnakeEffect
    , moveSnakeEffect
    , stepSnakeEffect
    )
    where

import Mambda.Snake
import Mambda.Utils

import Data.List (filter)
import Data.List.NonEmpty as NE (tail)

data GameStatus 
    = Running 
    | Paused 
    | Finished 
    deriving (Eq, Show)

data Game a = Game 
    { snake :: Snake a
    , snakeSpeed :: a
    , objects :: [Object a]
    , score :: Int
    , status :: GameStatus
    } deriving (Show, Eq)

newGame :: Snake a -> a -> [Object a] -> Game a
newGame snake speed objects = Game snake speed objects 0 Running

gameStatus :: Game a -> GameStatus
gameStatus Game { status = status } = status

gameSnake :: Game a -> Snake a
gameSnake Game{ snake = snake } = snake

gameScore :: Game a -> Int
gameScore Game { score = score } = score

type GameEffect a = Game a -> Game a

data Object a = Object
    { location :: a
    , collision :: GameEffect a
    } 

instance Show a => Show (Object a) where
    show (Object a _) = show a

instance Eq a => Eq (Object a) where
    (Object a _)  == (Object b _) = a == b

scoreGame :: GameEffect a
scoreGame g  = g { score = (+1) . score $ g }

gameObjects :: Game a -> [Object a]
gameObjects game@Game{ snake = snake, objects = objects } =
    snakeToObjects snake ++ objects

snakeToObjects :: Snake a -> [Object a]
snakeToObjects = fmap makeCollisionObject . snakeBody
  where
    snakeBody = NE.tail . body 
    makeCollisionObject loc = Object loc finishGame

finishGame :: GameEffect a
finishGame = changeStatus Finished

pauseGame :: GameEffect a
pauseGame = changeStatus Paused

resumeGame :: GameEffect a
resumeGame = changeStatus Running

changeStatus :: GameStatus -> GameEffect a
changeStatus status g = g { status = status }

moveSnake :: Monoid a => a -> Snake a -> Snake a
moveSnake vel snake = 
    move (getHead snake <> vel) snake

-- Lens
runningGameEffect :: GameEffect a -> GameEffect a
runningGameEffect f game@(Game _ _ _ _ Running) = f game
runningGameEffect _ game = game

modifySnake :: (Snake a -> Snake a) -> GameEffect a
modifySnake f g@Game{ snake = snake } = g { snake = f snake }

modifyObject :: (Object a -> Object a) -> GameEffect a
modifyObject f g@Game { objects = objects } = g { objects = f <$> objects }

growSnakeEffect :: PositiveInt -> GameEffect a
growSnakeEffect = runningGameEffect . modifySnake . increaseGrow

moveSnakeEffect :: Monoid a => a -> GameEffect a
moveSnakeEffect = runningGameEffect . modifySnake . moveSnake

stepSnakeEffect :: Monoid a => GameEffect a
stepSnakeEffect game@Game{ snakeSpeed = snakeSpeed } = moveSnakeEffect snakeSpeed game

changeSnakeSpeed :: a -> GameEffect a
changeSnakeSpeed speed = runningGameEffect $ \g -> g { snakeSpeed = speed }

replaceObject :: Eq a => a -> Object a -> GameEffect a
replaceObject loc obj =  runningGameEffect $ modifyObject $ \o -> if location o == loc then obj else o
