module Mambda.Console.Controls 
    where

import Mambda.Utils 
import Mambda.Flatland (Vec2D(..), east, west, north, south)
import Mambda (GameCommand(..))

import System.IO (hReady, stdin)
import Control.Concurrent (threadDelay)

import Data.Maybe (mapMaybe)
import Data.List (nub)

type Fps = PositiveInt

second :: Int
second = 1000000

readCommands :: Fps -> IO [GameCommand Vec2D]
readCommands (PositiveInt fps) = threadDelay delayInterval >> fmap parseCmds readStdin
 where
    delayInterval = second `div` fps
    parseCmds = nub . mapMaybe mapControls

readStdin :: IO String
readStdin = read' []
  where
    read' :: String -> IO String
    read' s = do
        ready <- hReady stdin
        if ready then getChar >>= \n -> read' (n:s) else return s

mapControls :: Char -> Maybe (GameCommand Vec2D)
-- WSAD
mapControls 'w' = Just $ ChangeSpeed north
mapControls 's' = Just $ ChangeSpeed south
mapControls 'a' = Just $ ChangeSpeed west
mapControls 'd' = Just $ ChangeSpeed east
-- Vi controls
mapControls 'k' = Just $ ChangeSpeed north
mapControls 'j' = Just $ ChangeSpeed south
mapControls 'h' = Just $ ChangeSpeed west
mapControls 'l' = Just $ ChangeSpeed east
mapControls 'p' = Just TogglePause
mapControls 'q' = Just Quit
mapControls _ = Nothing
