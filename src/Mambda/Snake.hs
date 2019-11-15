{-# LANGUAGE RecordWildCards #-}

module Mambda.Snake
    where

import Data.List.NonEmpty as NE

data Snake a = Snake 
    { growCount :: Int
    , body :: NE.NonEmpty a
    } deriving (Show, Eq)

move :: a -> Snake a -> Snake a
move newHead Snake{..} = Snake newGrowCount newBody
  where
    newGrowCount = max 0 $ growCount - 1
    newBody = if growCount > 0 then 
            newHead <| body
        else
            newHead :| NE.init body
