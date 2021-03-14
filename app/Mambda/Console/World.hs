module Mambda.Console.World
    ( Tile
    , createWorldMap

    , invisible 
    , visible

    , Glyphed(..)
    ) where

import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))
import Control.Applicative

import Mambda.Flatland (Vec2D(..))
import qualified Mambda.Flatland as Flatland 

import Mambda hiding (GameMonad(..))

data Glyphed a = Glyphed
    { glyph :: Maybe Char
    , position :: a
    } deriving (Show)

instance Eq a => Eq (Glyphed a) where
  (Glyphed _ a) == (Glyphed _ b) = a == b

invisible :: a -> Glyphed a
invisible = Glyphed Nothing

visible :: Char -> a -> Glyphed a
visible g = Glyphed (Just g)

instance Semigroup a => Semigroup (Glyphed a) where
    (Glyphed g1 loc1) <> (Glyphed g2 loc2) = Glyphed (g1 <|> g2) (loc1 <> loc2) 

instance Monoid a => Monoid (Glyphed a) where
    mempty = Glyphed Nothing mempty

type Tile = Glyphed Vec2D

createWorldMap :: Bool -> PositiveInt -> PositiveInt -> [Object Tile]
createWorldMap wrapMap height width = createWall <$> Flatland.createWorldMap height width
  where
    maxWidth = (getInt width) + 1
    maxHeight = (getInt height) + 1
    createWall (Vec2D (x,y)) = 
        if wrapMap then teleport wallGlyph teleportGlyph
                   else wall wallGlyph
      where
        glyph x y
            | (x,y) `elem` corners = '+'
            | y == maxWidth || y == 0 = '|'
            | x == maxHeight || x == 0 = '-'
        corners = [(x,y)| x <- [0, maxHeight], y <- [0, maxWidth]]
        wallGlyph = visible (glyph x y) $ Vec2D (x,y)
        foo m x 
          | x == 0 = m - 1
          | x == m = 1 - m
          | otherwise = 0
        teleportGlyph = invisible $ Vec2D (foo maxHeight x, foo maxWidth y)
