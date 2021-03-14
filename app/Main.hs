{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Mambda
import Mambda.Flatland

import Options

import Worlds.Console

import Data.Maybe (mapMaybe)
import Data.List (nub)
import qualified Data.Bifunctor as BF
import Control.Monad (void)
import Control.Arrow ((&&&))

import System.Console.ANSI (hideCursor, setTitle, setCursorPosition, clearScreen)
import System.IO (hSetEcho, stdin, hFlush, stdout, hReady, hSetBuffering, BufferMode(..))
import System.Random (newStdGen, randomRs)

import Control.Concurrent (threadDelay)

import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)

type Tile = Glyphed Vec2D

main :: IO ()
main = parseSettings >>= run
  where
    run = (runReaderT . runSnakeApp) startFlatGame

newtype SnakeApp a = SnakeApp { runSnakeApp :: ReaderT GameSettings IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

startFlatGame :: (Has GameSettings m, MonadIO m) => m ()
startFlatGame = do
    liftIO setupTerminal
    walls <- gameWalls
    sGlyph <- snakeGlyph <$> get
    void $ startGame walls (invisible south) (visible sGlyph (Vec2D (0,1))) foodLocations
  where
    setupTerminal = do
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        hideCursor
        setTitle "Mambda"
    gameWalls = do
        (height, (width, wrapMap)) <- (mapHeight &&& mapWidth &&& wrapMap) <$> get
        return $ generateWorldMap wrapMap height width
    foodLocations = do
        (width, height) <- both (flip (-) 1 . getInt) . (mapWidth &&& mapHeight) <$> get
        xs <- liftIO $ randomRs (1, height) <$> newStdGen
        ys <- liftIO $ randomRs (1, width) <$> newStdGen 
        return $ visible '@' . Vec2D <$> zip xs ys
      where
        both f = BF.bimap f f
        
class Has a m where
    get :: m a

instance Has GameSettings SnakeApp where
    get = SnakeApp ask

instance (Has GameSettings m, MonadIO m) => GameMonad m Tile where
    getCommands = do
        speed <- fps <$> get
        liftIO $ fmap (fmap invisible) <$> readCommands speed

    renderGame game = do
        (height, width) <- (mapHeight &&& mapWidth) <$> get
        liftIO $ do
            clearScreen
            mapM_ renderTile . fmap location . gameObjects $ game
            renderTile . getHead . gameSnake $ game
            setCursorPosition (getInt height + 2) 0
            putStr $ "Score " ++ show (gameScore game)
            let statusBar = 
                    case gameStatus game of
                        Finished -> "Game Over" 
                        Paused -> "Paused" 
                        _ -> ""
            statusBarText statusBar 1 (width, height)
            setCursorPosition (getInt height + 1 + 2) 0
            hFlush stdout
      where
        renderTile (Glyphed (Just g) (Vec2D (x,y))) = do
            setCursorPosition x y
            putChar g
        renderTile (Glyphed Nothing _) = return ()
        statusBarText text offset (x,y)= do
            setCursorPosition (getInt y + 2*offset) (getInt x + 2*offset - length text)
            putStr text

generateWorldMap :: Bool -> PositiveInt -> PositiveInt -> [Object Tile]
generateWorldMap wrapMap (PositiveInt height) (PositiveInt width) = 
    createWall <$> [(x,y) | x <- [0..height], y <- [0, maxWidth]] ++ [(x,y) | x <- [0, height + 1], y <- [0..maxWidth]]
  where
    maxWidth = width + 1
    maxHeight = height + 1
    createWall (x,y) = 
        if wrapMap then teleport wallGlyph teleportGlyph
                   else wall wallGlyph
      where
        glyph x y
            | (x,y) `elem` corners = '+'
            | y == (width + 1) || y == 0 = '|'
            | x == (height + 1) || x == 0 = '-'
        corners = [(x,y)| x <- [0, maxHeight], y <- [0, maxWidth]]
        wallGlyph = visible (glyph x y) $ Vec2D (x,y)
        foo m x 
          | x == 0 = m - 1
          | x == m = 1 - m
          | otherwise = 0
        teleportGlyph = invisible $ Vec2D (foo maxHeight x, foo maxWidth y)

-- Input
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
