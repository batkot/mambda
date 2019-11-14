{-# LANGUAGE RecordWildCards #-}

module Mambda 
    where

import Data.List.NonEmpty as NE
import Data.Bifunctor

data Direction 
    = North
    | South
    | West
    | East
    deriving (Show, Eq)

data Vec2 a = Vec2 a a deriving (Show, Eq)

data Snake = Snake 
    { direction :: Direction
    , body :: NE.NonEmpty (Vec2 Int)
    } deriving (Show, Eq)

move :: Snake -> Snake
move Snake{..} = Snake direction newBody
  where
    newHead = move' direction $ NE.head body
    newBody = newHead :| NE.init body

move' :: Num a => Direction -> Vec2 a -> Vec2 a
move' North (Vec2 x y) = Vec2 x (y + 1)
move' South (Vec2 x y) = Vec2 x (y - 1)
move' East (Vec2 x y) = Vec2 (x + 1) y
move' West (Vec2 x y) = Vec2 (x - 1) y

changeDirection :: Direction -> Snake -> Snake
changeDirection dir snake = snake { direction = dir }
