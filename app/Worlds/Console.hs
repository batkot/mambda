module Worlds.Console
    where

import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))
import Control.Applicative

import Mambda.Flatland

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
