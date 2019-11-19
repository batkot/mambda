module Options 
    ( Settings(..)
    , parseSettings )
    where

import Options.Applicative

parseSettings :: IO Settings
parseSettings = execParser opts 
  where
    opts = info (gameSettingsParser <**> helper)
        ( fullDesc
        <> progDesc "CLI classic Snake implementation"
        <> header "Mambda")

data Settings = Settings
    { mapHeight :: Int
    , mapWidth :: Int
    , fps :: Int
    , snakeGlyph :: Char
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
