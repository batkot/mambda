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

import Mambda.Widgets.ListZipper qualified as LZ

import Data.List.NonEmpty

import Data.Functor (($>))
import Data.Generics.Labels ()
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Extras
import Mambda.Game (PlayerInput (..), down, left, right, up)

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

data KeyBindings = KeyBindings
    { snakeUp :: Vty.Key
    , snakeDown :: Vty.Key
    , snakeLeft :: Vty.Key
    , snakeRight :: Vty.Key
    }
    deriving stock (Generic)

defaultKeyBindings :: KeyBindings
defaultKeyBindings =
    KeyBindings Vty.KUp Vty.KDown Vty.KLeft Vty.KRight

data Settings = Settings
    { worldSize :: WorldSize
    , snakeSpeed :: Integer
    , keyBindings :: KeyBindings
    }
    deriving stock (Generic)

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

data KeyPickerState = Picked Vty.Key | Selecting Vty.Key

keyPicker :: forall n ev. Vty.Key -> Lens' Settings Vty.Key -> SettingsPicker n ev
keyPicker def lens = SettingsPicker lens hmm (Picked def) render eventHandler
  where
    hmm (Picked key) = key
    hmm (Selecting key) = key
    render :: Bool -> KeyPickerState -> Brick.Widget n
    render True (Picked key) =
        Brick.str $ "| " <> Text.unpack (keyGlyph key) <> " |"
    render False (Picked key) =
        Brick.str . Text.unpack . keyGlyph $ key
    render _ _ = Brick.str "Press key"
    eventHandler ev = do
        s <- Brick.get
        case (s, ev) of
            (Picked key, Brick.VtyEvent (Vty.EvKey Vty.KEnter [])) ->
                Brick.put (Selecting key) $> True
            (Selecting _, Brick.VtyEvent (Vty.EvKey key [])) ->
                Brick.put (Picked key) $> True
            _ -> pure False

keyGlyph :: Vty.Key -> Text.Text
keyGlyph Vty.KEsc = "Esc"
keyGlyph Vty.KUp = "↑"
keyGlyph Vty.KDown = "↓"
keyGlyph Vty.KLeft = "←"
keyGlyph Vty.KRight = "→"
keyGlyph (Vty.KChar ' ') = "Space"
keyGlyph (Vty.KChar c) = Text.pack [c]
keyGlyph Vty.KEnd = "End"
keyGlyph Vty.KDel = "Del"
keyGlyph Vty.KEnter = "Enter"
keyGlyph Vty.KBS = "Backspace"
keyGlyph other = Text.show other

initState :: State n ev
initState =
    State
        { tick = 0
        , settings =
            LZ.fromNonEmpty $
                Setting "World Size" (selectPicker Text.show (Small :| [Medium, Large]) #worldSize)
                    :| [ Setting "Snake speed" (selectPicker Text.show (1 :| [2 .. 10]) #snakeSpeed)
                       , Setting "Up" (keyPicker Vty.KUp (#keyBindings . #snakeUp))
                       , Setting "Down" (keyPicker Vty.KDown (#keyBindings . #snakeDown))
                       , Setting "Left" (keyPicker Vty.KLeft (#keyBindings . #snakeLeft))
                       , Setting "Right" (keyPicker Vty.KRight (#keyBindings . #snakeRight))
                       ]
        , opt = Settings Small 3 defaultKeyBindings
        }

handleEvent :: Brick.BrickEvent n e -> Brick.EventM n (State n e) (Maybe Settings)
handleEvent (Brick.AppEvent _) = do
    Brick.modify $ #tick %~ (+ 1)
    pure Nothing
handleEvent ev = do
    handled <- eventHandler ev
    if handled
        then pure Nothing
        else case ev of
            (Brick.VtyEvent (Vty.EvKey Vty.KEnter [])) -> Brick.gets $ Just . view #opt
            (Brick.VtyEvent (Vty.EvKey Vty.KDown [])) -> do
                Brick.modify $ #settings %~ LZ.next
                pure Nothing
            (Brick.VtyEvent (Vty.EvKey Vty.KUp [])) -> do
                Brick.modify $ #settings %~ LZ.previous
                pure Nothing
            _ -> pure Nothing

render :: State n ev -> Brick.Widget n
render state =
    Brick.center $
        Brick.vCenter $
            Table.renderTable $
                Table.surroundingBorder False $
                    Table.columnBorders False $
                        Table.rowBorders False $
                            Table.alignLeft 0 $
                                Table.alignCenter 1 $
                                    Table.table $
                                        LZ.renderZipper renderSetting $
                                            state ^. #settings
  where
    renderSetting :: Bool -> Setting n ev -> [Brick.Widget n]
    renderSetting focused Setting{label, picker = SettingsPicker{render, state}} = [Brick.padLeftRight 3 $ Brick.str $ Text.unpack label, render focused state]

attributeMap :: Brick.AttrMap
attributeMap =
    Brick.attrMap
        Vty.defAttr
        []
