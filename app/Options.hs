{-# LANGUAGE RecordWildCards #-}

module Options 
    ( GameConfig(..)
    , parseSettings )
    where

import Mambda.Utils

import Options.Applicative

parseSettings :: IO (Maybe GameConfig)
parseSettings = foo <$> execParser opts 
  where
    opts = info (gameSettingsParser <**> helper)
        ( fullDesc
        <> progDesc "CLI classic Snake implementation"
        <> header "Mambda")

data GameConfig = GameConfig
    { mapHeight :: PositiveInt
    , mapWidth :: PositiveInt
    , fps :: PositiveInt
    , snakeGlyph :: Char
    }

-- I'm sure this can be done better 
foo :: Settings -> Maybe GameConfig
foo Settings{..} = GameConfig 
    <$> createPositiveInt sHeight 
    <*> createPositiveInt sWidth
    <*> createPositiveInt sFps
    <*> pure sGlyph

data Settings = Settings
    { sHeight :: Int
    , sWidth :: Int
    , sFps :: Int
    , sGlyph :: Char
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
    <*> option auto
        ( long "speed"
        <> short 's'
        <> showDefault
        <> value 3
        <> help "Game speed defined as frames per second"
        )
    <*> option auto
        ( long "glyph"
        <> short 'g'
        <> showDefault
        <> value '#'
        <> help "Snake body glyph"
        )
