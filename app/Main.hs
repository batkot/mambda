{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Mambda

import Options

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

class Has a m where
    get :: m a

instance Has GameConfig SnakeApp where
    get = SnakeApp ask

instance (Has GameConfig m, MonadIO m) => GameMonad m Vec2D Direction2D where
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
            mapM_ (renderTile "@" offset) . fmap location . objects $ game
            mapM_ (renderTile [glyph] offset). body . snake $ game
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
        renderTile g offset (x,y) = do
            setCursorPosition (x + offset) (y + offset)
            putStr g
        statusBarText text offset (x,y)= do
            setCursorPosition (getInt y + 2*offset) (getInt x + 2*offset - length text)
            putStr text

    randomObject = do
        (width, height) <- both (flip (-) 1 . getInt) . (mapWidth &&& mapHeight) <$> get
        loc <- liftIO $ BF.second (fst . randomR (0, width - 1)) . randomR (0, height - 1) <$> newStdGen
        return $ food one loc
          where
            both f = BF.bimap f f

printFlatWorld :: MonadIO m => PositiveInt -> PositiveInt -> m ()
printFlatWorld (PositiveInt height) (PositiveInt width) =
    liftIO $ do
        mapM_ printMapTile [(x,y) | x <- [0..height], y <- [0, maxWidth]]
        mapM_ printMapTile [(x,y) | x <- [0, height + 1], y <- [0..maxWidth]]
  where
    maxWidth = width + 1
    maxHeight = height + 1
    printMapTile (x,y) = do
        setCursorPosition x y
        putStr $ glyph x y
      where
        glyph x y
            | (x,y) `elem` corners = "+"
            | y == (width + 1) || y == 0 = "|"
            | x == (height + 1) || x == 0 = "-"
        corners = [(x,y)| x <- [0, maxHeight], y <- [0, maxWidth]]

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
