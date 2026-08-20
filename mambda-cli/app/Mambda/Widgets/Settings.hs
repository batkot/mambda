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
import GHC.Generics (Generic)
import Linear (V2)
import Optics.Core

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

data WorldSize = Small | Medium | Large
    deriving (Bounded, Enum)

data Settings = Settings
    { worldSize :: WorldSize
    }
    deriving stock (Generic)

data SettingsPicker = forall setting internal. SettingsPicker
    { lens :: Lens' Settings setting
    , hmm :: internal -> setting
    , state :: internal
    , render :: setting -> internal -> Brick.Widget ()
    , eventHandler :: Brick.BrickEvent () () -> Brick.EventM () internal ()
    }

data Foo = Foo {yyy :: Settings, sets :: LZ.ListZipper SettingsPicker} deriving (Generic)

eventHandler :: Brick.BrickEvent () () -> Brick.EventM () Foo ()
eventHandler x = do
    SettingsPicker{lens, hmm, state, eventHandler = eh} <- Brick.gets $ LZ.current . view #sets
    newState <- fmap hmm $ Brick.nestEventM' state $ eh x
    Brick.modify $ \s -> s & #yyy %~ (lens .~ newState)
    pure ()

selectPicker :: NonEmpty a -> Lens' Settings a -> SettingsPicker
selectPicker options lens = SettingsPicker lens LZ.current (LZ.fromNonEmpty options) render eventHandler
  where
    eventHandler :: Brick.BrickEvent n e -> Brick.EventM n (LZ.ListZipper a) ()
    eventHandler (Brick.VtyEvent (Vty.EvKey Vty.KLeft [])) = Brick.modify LZ.next
    eventHandler (Brick.VtyEvent (Vty.EvKey Vty.KRight [])) = Brick.modify LZ.next
    eventHandler _ = pure ()
    render :: a -> LZ.ListZipper a -> Brick.Widget n
    render _ _ = Brick.str ""

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
