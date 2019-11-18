{-# LANGUAGE RecordWildCards #-}

module Mambda.Snake
    where

import Data.List.NonEmpty as NE(NonEmpty(..), (<|), init, head)

data Snake a = Snake 
    { growCount :: Int
    , body :: NE.NonEmpty a
    } deriving (Show, Eq)

createSnake :: a -> Snake a
createSnake a = Snake 0 $ a :| []

getHead :: Snake a -> a
getHead = NE.head . body

move :: a -> Snake a -> Snake a
move newHead Snake{..} = Snake newGrowCount newBody
  where
    newGrowCount = max 0 $ growCount - 1
    newBody 
      | growCount > 0 = newHead <| body
      | otherwise = newHead :| NE.init body
