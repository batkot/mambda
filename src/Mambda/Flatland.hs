{-# LANGUAGE PatternSynonyms #-}

module Mambda.Flatland 
    where

import Mambda.Rules (Geometry(..))
import Mambda.Utils (PositiveInt, getInt, pattern PositiveInt)
import Data.Bifunctor (bimap)

data Direction2D 
    = North
    | South
    | West
    | East
    deriving (Show,Eq)

type Vec2D = (Int,Int)

createModulusFlatlandGeometry :: PositiveInt -> PositiveInt -> Geometry Vec2D Direction2D
createModulusFlatlandGeometry (PositiveInt maxHeight) (PositiveInt maxWidth) = 
    Geometry $ bimap (flippedMod  maxHeight) (flippedMod maxWidth) `fmap2` moveFunc
  where
    moveFunc North (x,y) = (x-1, y)
    moveFunc South (x,y) = (x+1, y)
    moveFunc West (x,y) = (x, y-1)
    moveFunc East (x,y) = (x, y+1) 
    flippedMod = flip mod
    fmap2 = fmap . fmap
