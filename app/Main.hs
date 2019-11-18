module Main where

import Mambda
import Mambda.Snake
import Mambda.Flatland

import Control.Monad (void)

main :: IO ()
main = putStrLn "Mambda"

type Game2D = Game Vec2D Direction2D

-- Input
mapControls :: Char -> Maybe (GameCommand Direction2D)
mapControls 'w' = Just $ ChangeDirection North
mapControls 's' = Just $ ChangeDirection South
mapControls 'a' = Just $ ChangeDirection West
mapControls 'd' = Just $ ChangeDirection East
mapControls _ = Nothing
