{-# LANGUAGE TemplateHaskell #-}

module Mambda.MainMenu (
    State,
    initState,
    render,
    handleEvent,
    MenuItem (..),
) where

import Prelude

import Brick ((<=>))
import Brick qualified
import Brick.Widgets.Center qualified as Brick
import Graphics.Vty qualified as Vty

import Data.FileEmbed as FileEmbed
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

newtype State a = State
    { menuItems :: ListZipper a
    }
    deriving stock (Show, Eq, Ord)

initState :: NonEmpty a -> State a
initState = State . fromNonEmpty

logo :: Brick.Widget n
logo =
    Brick.str logoTxt
  where
    logoTxt = Text.unpack $ Text.decodeUtf8 $ $(FileEmbed.embedFileRelative "data/logo.txt")

handleEvent :: Brick.BrickEvent n () -> Brick.EventM n (State a) (Maybe a)
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KDown [])) = do
    Brick.modify $ \(State items) -> State $ next items
    pure Nothing
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KUp [])) = do
    Brick.modify $ \(State items) -> State $ previous items
    pure Nothing
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KEnter [])) = do
    State items <- Brick.get
    pure $ Just $ current items
handleEvent _ = pure Nothing

render :: (MenuItem a) => State a -> Brick.Widget n
render State{menuItems} =
    Brick.center $
        Brick.vCenter (logo <=> menu)
  where
    ListZipper prev curr next = menuItems
    menu = Brick.vBox $ fmap renderMenuItem prev <> [renderCurrentItem curr] <> fmap renderMenuItem next

renderMenuItem :: (MenuItem a) => a -> Brick.Widget n
renderMenuItem menuItem = Brick.str $ Text.unpack $ toMenuItem menuItem

renderCurrentItem :: (MenuItem a) => a -> Brick.Widget n
renderCurrentItem menuItem = Brick.str $ "> " <> Text.unpack (toMenuItem menuItem)
