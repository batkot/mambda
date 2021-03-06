module Mambda.Console.Rendering
    ( initialize
    , release

    , renderGame
    , RenderingHandle
    ) where

import Mambda.Console.World

import Mambda hiding (GameMonad(..))
import Mambda.Flatland (Vec2D(..))

import System.Console.ANSI (hideCursor, setTitle, setCursorPosition, clearScreen, showCursor)
import System.IO (hSetEcho, stdin, hFlush, stdout, hReady, hSetBuffering, BufferMode(..))

data RenderingHandle = RenderingHandle

initialize :: IO RenderingHandle
initialize = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hideCursor
    setTitle "Mambda"
    return RenderingHandle

release :: RenderingHandle -> IO ()
release _ = showCursor

renderGame :: PositiveInt -> PositiveInt -> Game Tile -> IO ()
renderGame (PositiveInt height) (PositiveInt width) game = do
    clearScreen
    mapM_ renderTile . fmap location . gameObjects $ game
    renderTile . getHead . gameSnake $ game
    setCursorPosition (height + wallsWidth) 0
    putStr $ "Score " ++ show (gameScore game)
    let statusBar = 
            case gameStatus game of
                Finished -> "Game Over" 
                Paused -> "Paused" 
                _ -> ""
    statusBarText statusBar (width, height)
    hFlush stdout
  where
    wallsWidth = 2
    renderTile (Glyphed (Just g) (Vec2D (x,y))) = do
        setCursorPosition x y
        putChar g
    renderTile (Glyphed Nothing _) = return ()
    statusBarText text (x,y)= do
        setCursorPosition (y + wallsWidth) (x + wallsWidth - length text)
        putStr text
