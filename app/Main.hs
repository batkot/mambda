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
import System.Random (newStdGen, randomR)

import Control.Concurrent (threadDelay)

import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)

type Tile = Glyphed Vec2D

main :: IO ()
main = parseSettings >>= run
  where
    run Nothing = putStrLn "Game size has to be positive number"
    run (Just config) = (runReaderT . runSnakeApp) startFlatGame config

newtype SnakeApp a = SnakeApp { runSnakeApp :: ReaderT GameConfig IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

startFlatGame :: (Has GameConfig m, MonadIO m) => m ()
startFlatGame = do
    liftIO setupTerminal
    walls <- gameWalls
    void $ startGame walls (invisible south) $ visible '#' $ Vec2D (1,1)
  where
    setupTerminal = do
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        hideCursor
        setTitle "Mambda"
    gameWalls = do
        (height, width) <- (mapHeight &&& mapWidth) <$> get
        return $ generateWorldMap height width

class Has a m where
    get :: m a

instance Has GameConfig SnakeApp where
    get = SnakeApp ask

instance (Has GameConfig m, MonadIO m) => GameMonad m Tile where
    getCommands = do
        speed <- fps <$> get
        liftIO $ fmap (fmap invisible) <$> readCommands speed

    renderGame game = do
        glyph <- snakeGlyph <$> get
        (height, width) <- (mapHeight &&& mapWidth) <$> get
        offset <- getInt . printOffset <$> get
        liftIO $ do
            clearScreen
            mapM_ renderTile . fmap location . objects $ game
            mapM_ renderTile . body . snake $ game
            setCursorPosition (getInt height + offset + 1) 0
            putStr $ "Score " ++ show (score game)
            let statusBar = 
                    case (pause game, finished game) of
                        (_, True) -> "Game Over" 
                        (True, _) -> "Paused" 
                        _ -> ""
            statusBarText statusBar offset (width, height)
            hFlush stdout
      where
        renderTile (Glyphed (Just g) (Vec2D (x,y))) = do
            setCursorPosition x y
            putStr [g]
        renderTile (Glyphed Nothing _) = return ()
        statusBarText text offset (x,y)= do
            setCursorPosition (getInt y + 2*offset) (getInt x + 2*offset - length text)
            putStr text

    randomObject = do
        (width, height) <- both (flip (-) 1 . getInt) . (mapWidth &&& mapHeight) <$> get
        loc <- liftIO $ Vec2D . BF.second (fst . randomR (0, width - 1)) . randomR (0, height - 1) <$> newStdGen
        return $ food one $ Glyphed (Just '@') loc
          where
            both f = BF.bimap f f

generateWorldMap :: PositiveInt -> PositiveInt -> [Object Tile]
generateWorldMap (PositiveInt height) (PositiveInt width) = 
    createWall <$> [(x,y) | x <- [0..height], y <- [0, maxWidth]] ++ [(x,y) | x <- [0, height + 1], y <- [0..maxWidth]]
  where
    maxWidth = width + 1
    maxHeight = height + 1
    createWall (x,y) = teleport wallGlyph teleportGlyph
        -- wall $ visible (glyph x y) $ Vec2D (x,y)
      where
        glyph x y
            | (x,y) `elem` corners = '+'
            | y == (width + 1) || y == 0 = '|'
            | x == (height + 1) || x == 0 = '-'
        corners = [(x,y)| x <- [0, maxHeight], y <- [0, maxWidth]]
        wallGlyph = visible (glyph x y) $ Vec2D (x,y)
        foo m x 
          | x == 0 = m
          | x == m = 1
          | otherwise = x
        teleportGlyph = visible '#' $ Vec2D (foo maxHeight x, foo maxWidth y)

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
mapControls _ = Nothing
