{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Mambda
import Mambda.Snake
import Mambda.Flatland
import Mambda.Utils

import Data.Maybe (maybeToList, fromMaybe)
import Control.Monad (void)

import System.Console.ANSI (hideCursor, setTitle, setCursorPosition, clearScreen)
import System.IO (hSetEcho, stdin, hFlush, stdout)

main :: IO ()
main = do
    hSetEcho stdin False
    hideCursor
    setTitle "Mambda"
    fromMaybe (return ()) game
  where
    pos10 = createPositiveInt 10
    geo = fmap (\x -> createModulusFlatlandGeometry x x) pos10
    game = fmap (\g -> void (startGame g South (1,1))) geo

type Game2D = Game Vec2D Direction2D

instance GameMonad IO Vec2D Direction2D where
    getCommands = fmap (maybeToList . mapControls) getChar
    renderGame g = do 
        clearScreen
        mapM_ renderTile . body . snake $ g
        hFlush stdout
      where
        renderTile (x,y) = do
            setCursorPosition x y
            putStr "#"

-- Input
mapControls :: Char -> Maybe (GameCommand Direction2D)
mapControls 'w' = Just $ ChangeDirection North
mapControls 's' = Just $ ChangeDirection South
mapControls 'a' = Just $ ChangeDirection West
mapControls 'd' = Just $ ChangeDirection East
mapControls _ = Nothing
