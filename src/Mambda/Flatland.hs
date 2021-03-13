{-# LANGUAGE PatternSynonyms #-}

module Mambda.Flatland 
    ( Vec2D(..)

    , north
    , south
    , east
    , west
    )
    where

import Data.Semigroup
import Data.Monoid

newtype Vec2D = Vec2D (Int,Int) deriving (Eq, Show)

north :: Vec2D
north = Vec2D (-1, 0)

south :: Vec2D 
south = Vec2D (1, 0)

west :: Vec2D
west = Vec2D (0, -1)

east :: Vec2D
east = Vec2D (0, 1)

instance Semigroup Vec2D where
    (Vec2D (x1,y1)) <> (Vec2D (x2, y2)) = Vec2D (x1 + x2, y1 + y2)

instance Monoid Vec2D where
    mempty = Vec2D (0,0)
