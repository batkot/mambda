{-# LANGUAGE TemplateHaskell #-}

module Mambda.Widgets.MainMenu (
    State,
    initState,
    render,
    handleEvent,
    MenuItem (..),
    attributeMap,
) where

import Prelude

import Brick ((<=>))
import Brick qualified
import Brick.Widgets.Center qualified as Brick
import Graphics.Vty qualified as Vty

import Data.FileEmbed as FileEmbed
import Data.List qualified as List
import Data.List.NonEmpty

import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

class MenuItem a where
    toMenuItem :: a -> Text.Text

data ListZipper a = ListZipper
    { previous :: ![a]
    , current :: !a
    , next :: ![a]
    }
    deriving stock (Show, Eq, Ord)

next :: ListZipper a -> ListZipper a
next x@(ListZipper _ _ []) = x
next (ListZipper p c (n : ns)) = ListZipper (c : p) n ns

previous :: ListZipper a -> ListZipper a
previous x@(ListZipper [] _ _) = x
previous (ListZipper (p : ps) c n) = ListZipper ps p (c : n)

current :: ListZipper a -> a
current ListZipper{current} = current

fromNonEmpty :: NonEmpty a -> ListZipper a
fromNonEmpty (x :| xs) = ListZipper [] x xs

data State a = State
    { tick :: Integer
    , menuItems :: ListZipper a
    }
    deriving stock (Show, Eq, Ord)

initState :: NonEmpty a -> State a
initState = State 0 . fromNonEmpty

logo :: Integer -> Brick.Widget n
logo offset =
    Brick.vBox logoLines
  where
    attrs = Prelude.drop (fromInteger offset) $ Prelude.cycle $ replicate 3 brightAttr <> replicate 3 greenAttr
    logoLine attr line = Brick.withAttr attr $ Brick.str $ Text.unpack line
    logoLines = Prelude.zipWith logoLine attrs $ Text.lines $ Text.decodeUtf8 $ $(FileEmbed.embedFileRelative "data/logo.txt")

handleEvent :: Brick.BrickEvent n e -> Brick.EventM n (State a) (Maybe a)
handleEvent (Brick.AppEvent _) = do
    Brick.modify $ \(State x y) -> State (x + 1 `mod` 6) y
    pure Nothing
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KDown [])) = do
    Brick.modify $ \(State x items) -> State x $ next items
    pure Nothing
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KUp [])) = do
    Brick.modify $ \(State x items) -> State x $ previous items
    pure Nothing
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KEnter [])) = do
    State x items <- Brick.get
    pure $ Just $ current items
handleEvent _ = pure Nothing

render :: (MenuItem a) => State a -> Brick.Widget n
render State{menuItems, tick} =
    Brick.center $
        Brick.vCenter (logo tick <=> menu)
  where
    ListZipper prev curr next = menuItems
    menu = Brick.vBox $ fmap renderMenuItem (List.reverse prev) <> [renderCurrentItem curr] <> fmap renderMenuItem next

renderMenuItem :: (MenuItem a) => a -> Brick.Widget n
renderMenuItem menuItem = Brick.str $ Text.unpack $ toMenuItem menuItem

renderCurrentItem :: (MenuItem a) => a -> Brick.Widget n
renderCurrentItem menuItem = Brick.str $ "> " <> Text.unpack (toMenuItem menuItem)

attributeMap :: Brick.AttrMap
attributeMap =
    Brick.attrMap
        Vty.defAttr
        [ (brightAttr, Brick.fg Vty.brightGreen)
        , (greenAttr, Brick.fg Vty.green)
        ]

brightAttr :: Brick.AttrName
brightAttr = Brick.attrName "bright"

greenAttr :: Brick.AttrName
greenAttr = Brick.attrName "green"
