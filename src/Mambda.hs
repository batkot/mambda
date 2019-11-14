{-# LANGUAGE RecordWildCards #-}

module Mambda 
    where

import Data.List.NonEmpty as NE
import Data.Bifunctor
import Data.Semigroup

data Snake a = Snake 
    { velocity :: a
    , body :: NE.NonEmpty a
    } deriving (Show, Eq)

move :: Semigroup a => Snake a -> Snake a
move Snake{..} = Snake velocity newBody
  where
    newHead = velocity <> NE.head body
    newBody = newHead :| NE.init body

grow :: Semigroup a => Snake a -> Snake a
grow Snake{..} = Snake velocity newBody
  where
    newHead = velocity <> NE.head body
    newBody = newHead <| body

changeDirection :: a -> Snake a -> Snake a
changeDirection v snake = snake { velocity = v }
