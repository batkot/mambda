{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mambda.Console 
    ( Has(..)
    , GameSettings(..)
    , runConsoleGame
    )
    where

import Mambda
import qualified Mambda.Console.World as World
import qualified Mambda.Console.Controls as Controls
import qualified Mambda.Console.Rendering as Rendering
import Mambda.Flatland (Vec2D(..), south)

import Control.Arrow ((&&&))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (bimap)
import System.Random (newStdGen, randomRs)

class Has a m where
    get :: m a

data GameSettings = GameSettings
    { mapHeight :: PositiveInt
    , mapWidth :: PositiveInt
    , fps :: PositiveInt
    , snakeGlyph :: Char
    , wrapMap :: Bool
    }

instance (Has GameSettings m, MonadIO m) => GameMonad m World.Tile where
    getCommands = do
        speed <- fps <$> get
        liftIO $ fmap (fmap World.invisible) <$> Controls.readCommands speed

    renderGame game = do
        (height, width) <- (mapHeight &&& mapWidth) <$> get
        liftIO $ Rendering.renderGame height width game

runConsoleGame :: (Has GameSettings m, MonadIO m) => m ()
runConsoleGame = do
    liftIO Rendering.initialize
    (height, (width, wrapMap)) <- (mapHeight &&& mapWidth &&& wrapMap) <$> get
    let walls = World.createWorldMap wrapMap height width
    sGlyph <- snakeGlyph <$> get
    void $ startGame walls (World.invisible south) (World.visible sGlyph (Vec2D (0,1))) $ foodLocations height width
  where
    foodLocations height width= do
        xs <- liftIO $ randomRs (1, getInt height) <$> newStdGen
        ys <- liftIO $ randomRs (1, getInt width) <$> newStdGen 
        return $ World.visible '@' . Vec2D <$> zip xs ys
      where
        both f = bimap f f
