module Mambda.Console.Graphics 
    where

import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))
import Control.Applicative

import System.Console.ANSI (hideCursor, setTitle, setCursorPosition, clearScreen)
import System.IO (hSetEcho, stdin, hFlush, stdout, hReady, hSetBuffering, BufferMode(..))

import Mambda.Flatland as Flatland
import Mambda

data Glyphed a = Glyphed
    { glyph :: Maybe Char
    , position :: a
    } deriving (Show)

instance Eq a => Eq (Glyphed a) where
  (Glyphed _ a) == (Glyphed _ b) = a == b

invisible :: a -> Glyphed a
invisible = Glyphed Nothing

visible :: Char -> a -> Glyphed a
visible g = Glyphed (Just g)

instance Semigroup a => Semigroup (Glyphed a) where
    (Glyphed g1 loc1) <> (Glyphed g2 loc2) = Glyphed (g1 <|> g2) (loc1 <> loc2) 

instance Monoid a => Monoid (Glyphed a) where
    mempty = Glyphed Nothing mempty

type Tile = Glyphed Vec2D

createWorldMap :: Bool -> PositiveInt -> PositiveInt -> [Object Tile]
createWorldMap wrapMap height width = createWall <$> Flatland.createWorldMap height width
  where
    maxWidth = (getInt width) + 1
    maxHeight = (getInt height) + 1
    createWall (Vec2D (x,y)) = 
        if wrapMap then teleport wallGlyph teleportGlyph
                   else wall wallGlyph
      where
        glyph x y
            | (x,y) `elem` corners = '+'
            | y == maxWidth || y == 0 = '|'
            | x == maxHeight || x == 0 = '-'
        corners = [(x,y)| x <- [0, maxHeight], y <- [0, maxWidth]]
        wallGlyph = visible (glyph x y) $ Vec2D (x,y)
        foo m x 
          | x == 0 = m - 1
          | x == m = 1 - m
          | otherwise = 0
        teleportGlyph = invisible $ Vec2D (foo maxHeight x, foo maxWidth y)

initialize :: IO ()
initialize = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hideCursor
    setTitle "Mambda"

renderGame :: PositiveInt -> PositiveInt -> Game Tile -> IO ()
renderGame (PositiveInt height) (PositiveInt width) game = do
    clearScreen
    mapM_ renderTile . fmap location . gameObjects $ game
    renderTile . getHead . gameSnake $ game
    setCursorPosition (height + 2) 0
    putStr $ "Score " ++ show (gameScore game)
    let statusBar = 
            case gameStatus game of
                Finished -> "Game Over" 
                Paused -> "Paused" 
                _ -> ""
    statusBarText statusBar 1 (width, height)
    setCursorPosition (height + 1 + 2) 0
    hFlush stdout
  where
    renderTile (Glyphed (Just g) (Vec2D (x,y))) = do
        setCursorPosition x y
        putChar g
    renderTile (Glyphed Nothing _) = return ()
    statusBarText text offset (x,y)= do
        setCursorPosition (y + 2*offset) (x + 2*offset - length text)
        putStr text
