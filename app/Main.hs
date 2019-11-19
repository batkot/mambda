{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Mambda
import Mambda.Snake
import Mambda.Flatland
import Mambda.Utils

import Data.Maybe (maybeToList, fromMaybe, mapMaybe)
import Data.List (nub)
import Control.Monad (void)

import System.Console.ANSI (hideCursor, setTitle, setCursorPosition, clearScreen)
import System.IO (hSetEcho, stdin, hFlush, stdout, hReady, hSetBuffering, BufferMode(..))

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async 

import Options.Applicative 

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (gameSettingsParser <**> helper)
        ( fullDesc
        <> progDesc "CLI classic Snake implementation"
        <> header "Mambda")
    run Settings{..} = fromMaybe sizeError $ game
      where
        sizeError = putStrLn "Game size has to be positive number"
        game = startFlatGame <$> createPositiveInt mapHeight <*> createPositiveInt mapWidth
        

startFlatGame :: PositiveInt -> PositiveInt -> IO ()
startFlatGame height width = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering 
    hideCursor
    setTitle "Mambda"
    game
  where
    geo = createModulusFlatlandGeometry height width
    game = void $ startGame geo South (1,1)

type Game2D = Game Vec2D Direction2D

instance GameMonad IO Vec2D Direction2D where
    getCommands = readCommands 3 
    renderGame g = do 
        clearScreen
        mapM_ renderTile . body . snake $ g
        hFlush stdout
      where
        renderTile (x,y) = do
            setCursorPosition x y
            putStr "#"
-- Settings
data Settings = Settings
    { mapHeight :: Int
    , mapWidth :: Int
    }

gameSettingsParser :: Parser Settings
gameSettingsParser = Settings 
    <$> option auto
        ( long "height"
        <> short 'h'
        <> showDefault
        <> value 20 
        <> help "Height of game world"
        )
    <*> option auto
        ( long "width"
        <> short 'w'
        <> showDefault
        <> value 30 
        <> help "Width of game world"
        )

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
mapControls 'w' = Just $ ChangeDirection North
mapControls 's' = Just $ ChangeDirection South
mapControls 'a' = Just $ ChangeDirection West
mapControls 'd' = Just $ ChangeDirection East
mapControls _ = Nothing
