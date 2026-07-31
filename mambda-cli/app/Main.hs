{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Prelude

import Brick ((<+>))
import Brick qualified
import Brick.Widgets.Border qualified as Brick
import Brick.Widgets.Border.Style qualified as Brick
import Brick.Widgets.Center qualified as Brick

import Data.FileEmbed as FileEmbed

import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

logo :: Brick.Widget ()
logo =
    Brick.str logoTxt
  where
    logoTxt = Text.unpack $ Text.decodeUtf8 $ $(FileEmbed.embedFileRelative "data/logo.txt")

main :: IO ()
main =
    Brick.simpleMain ui
  where
    ui :: Brick.Widget ()
    ui =
        Brick.center $
            Brick.joinBorders $
                Brick.withBorderStyle Brick.unicode $
                    Brick.borderWithLabel (Brick.str "Mambda") logo
