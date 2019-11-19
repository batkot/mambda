{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Mambda
import Mambda.Snake
import Mambda.Flatland
import Mambda.Utils

import Options 

import Data.Maybe (maybeToList, fromMaybe, mapMaybe)
import Data.List (nub)
import Control.Monad (void)

import System.Console.ANSI (hideCursor, setTitle, setCursorPosition, clearScreen)
import System.IO (hSetEcho, stdin, hFlush, stdout, hReady, hSetBuffering, BufferMode(..))

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async 

import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)
import Control.Monad.Trans.Class (lift)

main :: IO ()
main = parseSettings >>= run
  where
    run settings@Settings{..} = fromMaybe sizeError $ x
      where
        sizeError = putStrLn "Game size has to be positive number"
        game = startFlatGame <$> createPositiveInt mapHeight <*> createPositiveInt mapWidth
        x = runReaderT . runSnakeApp <$> game <*> pure settings

startFlatGame :: PositiveInt -> PositiveInt -> SnakeApp ()
startFlatGame height width = do
    SnakeApp $ lift $ setupTerminal
    game
  where
    setupTerminal = do
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering 
        hideCursor
        setTitle "Mambda"
    geo = createModulusFlatlandGeometry height width
    game = void $ startGame geo South (1,1)

newtype SnakeApp a = SnakeApp { runSnakeApp :: ReaderT Settings IO a }
    deriving (Functor, Applicative, Monad)

instance GameMonad SnakeApp Vec2D Direction2D where
    getCommands = SnakeApp $ do
        speed <- fps <$> ask 
        lift $ readCommands speed
    renderGame game = SnakeApp $ do
        glyph <- snakeGlyph <$> ask
        lift $ do
            clearScreen
            mapM_ (renderTile [glyph]). body . snake $ game
            hFlush stdout
      where
        renderTile g (x,y) = do
            setCursorPosition x y
            putStr g

-- Input
type Fps = Int

readCommands :: Fps -> IO [GameCommand Direction2D]
readCommands x = threadDelay delayInterval >> fmap parseCmds readStdin
 where
    delayInterval = 1000000 `div` x
    parseCmds = nub . mapMaybe mapControls

readStdin :: IO String
readStdin = read' []
  where
    read' :: String -> IO String
    read' s = do 
        ready <- hReady stdin
        if ready then getChar >>= \n -> read' (n:s) else return s

mapControls :: Char -> Maybe (GameCommand Direction2D)
-- WSAD
mapControls 'w' = Just $ ChangeDirection North
mapControls 's' = Just $ ChangeDirection South
mapControls 'a' = Just $ ChangeDirection West
mapControls 'd' = Just $ ChangeDirection East
-- Vi controls
mapControls 'k' = Just $ ChangeDirection North
mapControls 'j' = Just $ ChangeDirection South
mapControls 'h' = Just $ ChangeDirection West
mapControls 'l' = Just $ ChangeDirection East
mapControls 'p' = Just TogglePause
mapControls _ = Nothing
