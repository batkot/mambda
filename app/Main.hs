{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Options (parseSettings)
import Mambda.Console
import Mambda.Console.Rendering as R

import Control.Exception (bracket)
import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)


main :: IO ()
main = parseSettings >>= run >> putStrLn ""
  where
    run = bracket R.initialize R.release . runGame
    runGame = flip $ runReaderT . runSnakeApp . runConsoleGame

newtype SnakeApp a = SnakeApp { runSnakeApp :: ReaderT GameSettings IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance Has GameSettings SnakeApp where
    get = SnakeApp ask
