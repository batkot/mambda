{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Mambda
import Mambda.Console.Controls
import Mambda.Flatland
import Options
import Mambda.Console.Rendering as Rendering
import Mambda.Console.Graphics as Console

import qualified Data.Bifunctor as BF
import Control.Monad (void)
import Control.Arrow ((&&&))

import System.Random (newStdGen, randomRs)

import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)


main :: IO ()
main = parseSettings >>= run >> putStrLn ""
  where
    run = (runReaderT . runSnakeApp) startFlatGame

newtype SnakeApp a = SnakeApp { runSnakeApp :: ReaderT GameSettings IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

startFlatGame :: (Has GameSettings m, MonadIO m) => m ()
startFlatGame = do
    liftIO Rendering.initialize
    walls <- gameWalls
    sGlyph <- snakeGlyph <$> get
    void $ startGame walls (invisible south) (visible sGlyph (Vec2D (0,1))) foodLocations
  where
    gameWalls = do
        (height, (width, wrapMap)) <- (mapHeight &&& mapWidth &&& wrapMap) <$> get
        return $ Console.createWorldMap wrapMap height width
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
        liftIO $ Rendering.renderGame height width game
