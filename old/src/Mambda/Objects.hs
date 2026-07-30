module Mambda.Objects 
    ( food
    , wall
    , teleport
    ) where

import Mambda.Rules
import Mambda.Utils

food :: GameTile a => PositiveInt -> a -> [a] -> Object a
food grow loc [] = Object loc $ scoreGame . finishGame
food grow loc (x:xs) = Object loc 
    $ scoreGame 
    . growSnakeEffect grow 
    . replaceObject loc newFood
  where
    newFood = food grow x xs

wall :: a -> Object a
wall loc = Object loc finishGame

teleport :: GameTile a => a -> a -> Object a
teleport loc movement = Object loc $ moveSnakeEffect movement
