module Options 
    ( GameSettings(..)
    , parseSettings )
    where

import Mambda.Utils
import Data.Maybe (fromJust)

import Options.Applicative

parseSettings :: IO GameSettings
parseSettings = execParser opts 
  where
    opts = info (gameSettingsParser <**> helper)
        ( fullDesc
        <> progDesc "CLI classic Snake implementation"
        <> header "Mambda")

data GameSettings = GameSettings
    { mapHeight :: PositiveInt
    , mapWidth :: PositiveInt
    , fps :: PositiveInt
    , snakeGlyph :: Char
    , wrapMap :: Bool
    }

gameSettingsParser :: Parser GameSettings
gameSettingsParser = GameSettings 
    <$> option positiveInt 
        ( long "height"
        <> short 'h'
        <> showDefault
        <> (value . forcePositiveInt) 20
        <> help "Height of game world"
        )
    <*> option positiveInt
        ( long "width"
        <> short 'w'
        <> showDefault
        <> (value . forcePositiveInt) 30
        <> help "Width of game world"
        )
    <*> option positiveInt
        ( long "speed"
        <> short 's'
        <> showDefault
        <> (value . forcePositiveInt) 6
        <> help "Game speed defined as frames per second"
        )
    <*> option auto
        ( long "glyph"
        <> short 'g'
        <> showDefault
        <> value '#'
        <> help "Snake body glyph"
        )
    <*> switch
        ( long "wrap-map"
        <> short 'w'
        <> showDefault
        <> help "Hitting walls sends snake to other side of the map"
        )

positiveInt :: ReadM PositiveInt
positiveInt = eitherReader $ toError "Has to be positive int" . createPositiveInt . read

-- partial function :(
forcePositiveInt :: Int -> PositiveInt
forcePositiveInt = fromJust . createPositiveInt

toError :: err -> Maybe a -> Either err a
toError _ (Just a) = Right a
toError err _ = Left err
