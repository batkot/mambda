{-# LANGUAGE PatternSynonyms #-}

module Mambda.Utils 
    ( PositiveInt
    , getInt
    , createPositiveInt
    , pattern PositiveInt
    , one
    )
    where

newtype PositiveInt = MkPositiveInt Int deriving (Show, Eq)

getInt :: PositiveInt -> Int
getInt (MkPositiveInt x) = x

createPositiveInt :: Int -> Maybe PositiveInt
createPositiveInt x 
    | x > 0 = Just $ MkPositiveInt x
    | otherwise = Nothing

pattern PositiveInt :: Int -> PositiveInt
pattern PositiveInt x <- MkPositiveInt x

one :: PositiveInt
one = MkPositiveInt 1
