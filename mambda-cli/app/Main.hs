{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Prelude

import Brick qualified
import Brick.Widgets.Border qualified as Brick
import Brick.Widgets.Border.Style qualified as Brick
import Brick.Widgets.Center qualified as Brick
import Graphics.Vty qualified as Vty

import Data.FileEmbed as FileEmbed

import Control.Monad (void)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

logo :: Brick.Widget n
logo =
    Brick.str logoTxt
  where
    logoTxt = Text.unpack $ Text.decodeUtf8 $ $(FileEmbed.embedFileRelative "data/logo.txt")

data MambdaCliState
    = Menu
    deriving stock (Show, Eq, Ord)

data MambdaCliResource = MambdaCliResource
    deriving stock (Show, Eq, Ord)

main :: IO ()
main =
    void $ Brick.defaultMain app Menu
  where
    app :: Brick.App MambdaCliState () MambdaCliResource
    app =
        Brick.App
            { appDraw = const [ui]
            , appChooseCursor = \_ _ -> Nothing
            , appHandleEvent = handleEvent
            , appStartEvent = pure ()
            , appAttrMap = const $ Brick.attrMap Vty.defAttr []
            }
    ui :: Brick.Widget n
    ui =
        Brick.center $
            Brick.joinBorders $
                Brick.withBorderStyle Brick.unicode $
                    Brick.borderWithLabel (Brick.str "Mambda") logo
    handleEvent :: Brick.BrickEvent MambdaCliResource () -> Brick.EventM MambdaCliResource MambdaCliState ()
    handleEvent (Brick.VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = Brick.halt
    handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KEsc [])) = Brick.halt
    handleEvent _ = pure ()
