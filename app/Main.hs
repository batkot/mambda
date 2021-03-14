{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Mambda
import Mambda.Console.Controls
import Mambda.Flatland
import Options
import Mambda.Console.Rendering as Rendering
import Mambda.Console.Graphics as Console
import Mambda.Console

import qualified Data.Bifunctor as BF
import Control.Arrow ((&&&))


import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)


main :: IO ()
main = parseSettings >>= run >> putStrLn ""
  where
    run = (runReaderT . runSnakeApp) runConsoleGame

newtype SnakeApp a = SnakeApp { runSnakeApp :: ReaderT GameSettings IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance Has GameSettings SnakeApp where
    get = SnakeApp ask
