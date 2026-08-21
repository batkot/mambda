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

import Data.Functor (($>))
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Optics.Core

data Setting n ev = Setting
    { label :: Text.Text
    , picker :: SettingsPicker n ev
    }
    deriving stock (Generic)

data State n ev = State
    { tick :: Integer
    , settings :: LZ.ListZipper (Setting n ev)
    , opt :: Settings
    }
    deriving stock (Generic)

data WorldSize = Small | Medium | Large
    deriving (Show, Ord, Eq, Bounded, Enum)

data Settings = Settings
    { worldSize :: WorldSize
    , snakeSpeed :: Integer
    }
    deriving stock (Show, Eq, Ord, Generic)

data SettingsPicker n ev = forall setting internal. SettingsPicker
    { lens :: Lens' Settings setting
    , hmm :: internal -> setting
    , state :: internal
    , render :: Bool -> internal -> Brick.Widget n
    , eventHandler :: Brick.BrickEvent n ev -> Brick.EventM n internal Bool
    }

eventHandler :: Brick.BrickEvent n ev -> Brick.EventM n (State n ev) Bool
eventHandler x = do
    SettingsPicker{lens, hmm, state, eventHandler = eh, render} <- Brick.gets $ view #picker . LZ.current . view #settings
    (newInternal, handled) <- Brick.nestEventM state $ eh x
    let newPicker = SettingsPicker{lens, hmm, state = newInternal, eventHandler = eh, render}
    Brick.modify $ \s -> s & #opt %~ (lens .~ hmm newInternal) & #settings %~ LZ.modifyCurrent (#picker .~ newPicker)
    pure handled

selectPicker :: forall a n ev. (a -> Text.Text) -> NonEmpty a -> Lens' Settings a -> SettingsPicker n ev
selectPicker f options lens = SettingsPicker lens LZ.current (LZ.fromNonEmpty options) render eventHandler
  where
    eventHandler :: Brick.BrickEvent n e -> Brick.EventM n (LZ.ListZipper a) Bool
    eventHandler (Brick.VtyEvent (Vty.EvKey Vty.KRight [])) = Brick.modify LZ.next $> True
    eventHandler (Brick.VtyEvent (Vty.EvKey Vty.KLeft [])) = Brick.modify LZ.previous $> True
    eventHandler _ = pure False
    render :: Bool -> LZ.ListZipper a -> Brick.Widget n
    render False x = Brick.padLeftRight 2 $ Brick.str . Text.unpack . f . LZ.current $ x
    render True x = Brick.str $ "< " <> Text.unpack (f (LZ.current x)) <> " >"

initState :: State n ev
initState =
    State
        { tick = 0
        , settings =
            LZ.fromNonEmpty $
                Setting "World Size" (selectPicker Text.show (Small :| [Medium, Large]) #worldSize)
                    :| [ Setting "Snake speed" (selectPicker Text.show (1 :| [2 .. 10]) #snakeSpeed)
                       ]
        , opt = Settings Small 3
        }

handleEvent :: Brick.BrickEvent n e -> Brick.EventM n (State n e) Bool
handleEvent (Brick.AppEvent _) = do
    Brick.modify $ #tick %~ (+ 1)
    pure False
handleEvent ev = do
    handled <- eventHandler ev
    if handled
        then pure False
        else case ev of
            (Brick.VtyEvent (Vty.EvKey Vty.KEnter [])) -> pure True
            (Brick.VtyEvent (Vty.EvKey Vty.KDown [])) -> do
                Brick.modify $ #settings %~ LZ.next
                pure False
            (Brick.VtyEvent (Vty.EvKey Vty.KUp [])) -> do
                Brick.modify $ #settings %~ LZ.previous
                pure False
            _ -> pure False

render :: State n ev -> Brick.Widget n
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
                                        LZ.renderZipper renderSetting settings
  where
    renderSetting :: Bool -> Setting n ev -> [Brick.Widget n]
    renderSetting focused Setting{label, picker = SettingsPicker{render, state}} = [Brick.padLeftRight 3 $ Brick.str $ Text.unpack label, render focused state]

attributeMap :: Brick.AttrMap
attributeMap =
    Brick.attrMap
        Vty.defAttr
        []
