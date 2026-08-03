{-# LANGUAGE TemplateHaskell #-}

module Mambda.MainMenu (
    State,
    initState,
    render,
    handleEvent,
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

data MenuItem = MenuItem
    { label :: !Text.Text
    }
    deriving stock (Show, Eq, Ord)

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

fromNonEmpty :: NonEmpty a -> ListZipper a
fromNonEmpty (x :| xs) = ListZipper [] x xs

newtype State = State
    { menuItems :: ListZipper MenuItem
    }
    deriving stock (Show, Eq, Ord)

initState :: State
initState = State $ fromNonEmpty $ MenuItem <$> "New Game" :| ["Quit"]

logo :: Brick.Widget n
logo =
    Brick.str logoTxt
  where
    logoTxt = Text.unpack $ Text.decodeUtf8 $ $(FileEmbed.embedFileRelative "data/logo.txt")

handleEvent :: Brick.BrickEvent n () -> Brick.EventM n State ()
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KDown [])) =
    Brick.modify $ \(State items) -> State $ next items
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KUp [])) =
    Brick.modify $ \(State items) -> State $ previous items
handleEvent _ = pure ()

render :: State -> Brick.Widget n
render State{menuItems} =
    Brick.center $
        Brick.vCenter (logo <=> menu)
  where
    ListZipper prev curr next = menuItems
    menu = Brick.vBox $ fmap renderMenuItem prev <> [renderCurrentItem curr] <> fmap renderMenuItem next

renderMenuItem :: MenuItem -> Brick.Widget n
renderMenuItem MenuItem{label} = Brick.str $ Text.unpack label

renderCurrentItem :: MenuItem -> Brick.Widget n
renderCurrentItem MenuItem{label} = Brick.str $ "> " <> Text.unpack label
