{-# LANGUAGE TemplateHaskell #-}

module Mambda.Widgets.Settings (
    State,
    initState,
    render,
    handleEvent,
    attributeMap,
) where

import Prelude

import Brick ((<=>))
import Brick qualified
import Brick.Widgets.Center qualified as Brick
import Brick.Widgets.Table qualified as Table
import Graphics.Vty qualified as Vty

import Data.FileEmbed as FileEmbed
import Data.List qualified as List
import Data.List.NonEmpty

import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

newtype State = State
    { message :: Text.Text
    }
    deriving stock (Show, Eq, Ord)

-- Settings
-- Start Speed?
-- Keybindings
-- Snake(s)
-- Color
-- Up, down, left, right, fire
-- Game
-- Pause, Quit

initState :: State
initState = State "YO"

handleEvent :: Brick.BrickEvent n e -> Brick.EventM n State Bool
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KEnter [])) = pure True
handleEvent _ = pure False

render :: State -> Brick.Widget n
render State{message} =
    Brick.center $
        Brick.vCenter $
            Table.renderTable foo

foo :: Table.Table n
foo =
    Table.surroundingBorder False $
        Table.columnBorders False $
            Table.rowBorders False $
                Table.table
                    [ [Brick.str "Snake Speed", Brick.str "3"]
                    , [Brick.str "Snake Color", Brick.str "Green"]
                    , [Brick.str "Controls", Brick.str ""]
                    , [Brick.str "Up", Brick.str "Arrow Up"]
                    , [Brick.str "Down", Brick.str "Arrow Down"]
                    , [Brick.str "Left", Brick.str "Arrow Left"]
                    , [Brick.str "Right", Brick.str "Arrow Right"]
                    ]

attributeMap :: Brick.AttrMap
attributeMap =
    Brick.attrMap
        Vty.defAttr
        []
