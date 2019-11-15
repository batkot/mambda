{-# LANGUAGE LambdaCase #-}
module Main where

import Mambda
import Mambda.Snake
import Control.Monad (void)

main :: IO ()
main = void $ loop startGame 

type Game2D = Game Vec2D Direction2D

loop :: Game2D -> IO Game2D
loop game = 
    (putStrLn . show . snake) game
    >> (putStrLn . show . snakeDirection) game
    >> fmap mapControls getChar 
    >>= \case
        Nothing -> loop game
        Just cmd -> loop $ (flip updateState) Tick $ updateState game cmd
    

mapControls :: Char -> Maybe (GameCommand Direction2D)
mapControls 'w' = Just $ ChangeDirection North
mapControls 's' = Just $ ChangeDirection South
mapControls 'a' = Just $ ChangeDirection West
mapControls 'd' = Just $ ChangeDirection East
mapControls _ = Nothing

data Direction2D 
    = North
    | South
    | West
    | East
    deriving (Show, Eq)

type Vec2D = (Int,Int)

createModulus2DTopology :: Int -> Int -> Topology Vec2D Direction2D
createModulus2DTopology maxWidth maxHeight = 
    Topology moveFunc
  where
    moveFunc North (x,y) = ((x-1) `mod` maxHeight, y)
    moveFunc South (x,y) = ((x+1) `mod` maxHeight, y)
    moveFunc West (x,y) = (x, (y-1) `mod` maxWidth)
    moveFunc East (x,y) = (x, (y+1) `mod` maxWidth)

startGame :: Game2D
startGame = Game snake startDir modTop
  where
    snake = createSnake (1,1)
    startDir = South
    modTop = createModulus2DTopology 10 10
    
