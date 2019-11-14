module Mambda.Worlds 
    ( RangedSum
    , lowerBound
    , upperBound
    , value
    , mkRangedSum
    )
    where

import Data.Semigroup

data RangedSum a = RangedSum a a a deriving (Show, Eq)

lowerBound :: RangedSum a -> a
lowerBound (RangedSum a _ _) = a

upperBound :: RangedSum a -> a
upperBound (RangedSum _ a _) = a

value :: RangedSum a -> a
value (RangedSum _ _ a) = a

mkRangedSum 
    :: Ord a 
    => a  -- lowerBound
    -> a  -- upperBound
    -> a  -- value
    -> Maybe (RangedSum a)
mkRangedSum lower upper val 
    | lower > upper = Nothing
    | otherwise = Just $ RangedSum lower upper $ min upper . max lower $ val

instance Integral a => Semigroup (RangedSum a) where
    (RangedSum min1 max1 x) <> (RangedSum min2 max2 y) =
        RangedSum newMin newMax val
      where
        newMin = min min1 min2
        newMax = max max1 max2
        range = newMax - newMin 
        val = (x + y `mod` range) + newMin
