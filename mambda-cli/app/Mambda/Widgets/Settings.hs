{-# LANGUAGE TemplateHaskell #-}

module Mambda.Widgets.Settings (
    State,
    initState,
    render,
    handleEvent,
    attributeMap,
) where

import Prelude

import Brick qualified
import Brick.Widgets.Center qualified as Brick
import Brick.Widgets.Table qualified as Table
import Graphics.Vty qualified as Vty

import Mambda.Widgets.Cursor qualified as Cursor
import Mambda.Widgets.ListZipper qualified as LZ

import Data.List.NonEmpty

import Data.Text qualified as Text
import Linear (V2)

data Setting = Setting
    { label :: Text.Text
    , value :: Text.Text
    }
    deriving stock (Show, Eq, Ord)

data State = State
    { tick :: Integer
    , settings :: LZ.ListZipper Setting
    }
    deriving stock (Show, Eq, Ord)

data Settings = Settings
    { worldSize :: V2 Integer
    , foo :: ()
    }

-- Settings
-- Start Speed?
-- Keybindings
-- Snake(s)
-- Color
-- Up, down, left, right, fire
-- Game
-- Pause, Quit

initState :: State
initState =
    State 0 $
        LZ.fromNonEmpty $
            Setting{label = "Snake Speed", value = "3"}
                :| [ Setting{label = "Snake Color", value = "Green"}
                   , Setting{label = "Controls", value = ""}
                   , Setting{label = "Up", value = "Arrow Up"}
                   , Setting{label = "Down", value = "Arrow Down"}
                   , Setting{label = "Left", value = "Arrow Left"}
                   , Setting{label = "Right", value = "Arrow Right"}
                   ]

handleEvent :: Brick.BrickEvent n e -> Brick.EventM n State Bool
handleEvent (Brick.AppEvent _) = do
    Brick.modify $ \(State{tick, settings}) -> State (tick + 1) settings
    pure False
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KEnter [])) = pure True
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KDown [])) = do
    Brick.modify $ \(State{tick, settings}) -> State tick $ LZ.next settings
    pure False
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KUp [])) = do
    Brick.modify $ \(State{tick, settings}) -> State tick $ LZ.previous settings
    pure False
handleEvent _ = pure False

render :: State -> Brick.Widget n
render State{tick, settings} =
    Brick.center $
        Brick.vCenter $
            Table.renderTable $
                Table.surroundingBorder False $
                    Table.columnBorders False $
                        Table.rowBorders False $
                            Table.alignLeft 0 $
                                Table.alignRight 2 $
                                    Table.table $
                                        LZ.renderZipper renderTableRow settings
  where
    renderTableRow True Setting{label, value} = [Cursor.cursorFrame tick, Brick.str (Text.unpack label), Brick.str (Text.unpack value)]
    renderTableRow False Setting{label, value} = [Brick.str "  ", Brick.str (Text.unpack label), Brick.str (Text.unpack value)]

attributeMap :: Brick.AttrMap
attributeMap =
    Brick.attrMap
        Vty.defAttr
        []
