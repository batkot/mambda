{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Mambda
import Mambda.Snake
import Mambda.Flatland
import Mambda.Utils

import Options 

import Data.Maybe (mapMaybe)
import Data.List (nub)
import Control.Monad (void)
import Control.Arrow ((&&&))

import System.Console.ANSI (hideCursor, setTitle, setCursorPosition, clearScreen)
import System.IO (hSetEcho, stdin, hFlush, stdout, hReady, hSetBuffering, BufferMode(..))

import Control.Concurrent (threadDelay)

import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)

main :: IO ()
main = parseSettings >>= run
  where
    run Nothing = putStrLn "Game size has to be positive number"
    run (Just config) = (runReaderT . runSnakeApp) startFlatGame config

newtype SnakeApp a = SnakeApp { runSnakeApp :: ReaderT GameConfig IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

startFlatGame :: (Has m GameConfig, MonadIO m) => m ()
startFlatGame = do
    liftIO setupTerminal
    geo <- geo
    void $ startGame geo South (1,1)
  where
    setupTerminal = do
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering 
        hideCursor
        setTitle "Mambda"
    geo = do 
        height <- mapHeight <$> get
        width <- mapWidth <$> get
        return $ createModulusFlatlandGeometry height width

printFlatWorld :: MonadIO m => PositiveInt -> PositiveInt -> m ()
printFlatWorld (PositiveInt height) (PositiveInt width) = 
    liftIO $ do
        mapM_ printMap [(x,y) | x <- [0..height], y <- [0, maxWidth]]
        mapM_ printMap [(x,y) | x <- [0, height + 1], y <- [0..maxWidth]]
  where
    maxWidth = width + 1
    maxHeight = height + 1
    printMap (x,y) = do
        setCursorPosition x y
        putStr $ glyph x y
      where
        glyph x y 
            | (x,y) `elem` corners = "+"
            | y == (width + 1) || y == 0 = "|"
            | x == (height + 1) || x == 0 = "-"
        corners = [(x,y)| x <- [0, maxHeight], y <- [0, maxWidth]]

class Has m a where
    get :: m a

instance Has SnakeApp GameConfig where
    get = SnakeApp ask

instance (Has m GameConfig, MonadIO m) => GameMonad m Vec2D Direction2D where
    getCommands = do
        speed <- fps <$> get
        liftIO $ readCommands speed

    renderGame game = do
        glyph <- snakeGlyph <$> get
        (height, width) <- (mapHeight &&& mapWidth) <$> get
        offset <- getInt . printOffset <$> get
        liftIO $ do
            clearScreen
            printFlatWorld height width
            mapM_ (renderTile [glyph] offset). body . snake $ game
            hFlush stdout
      where
        renderTile g offset (x,y) = do
            setCursorPosition (x + offset) (y + offset)
            putStr g

-- Input
type Fps = PositiveInt

second :: Int
second = 1000000

readCommands :: Fps -> IO [GameCommand Direction2D]
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
