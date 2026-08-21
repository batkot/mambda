module Mambda.Widgets.ListZipper (
    ListZipper,
    next,
    current,
    previous,
    renderZipper,
    fromNonEmpty,
    swapCurrent,
    modifyCurrent,
) where

import Prelude

import Data.List qualified as List
import Data.List.NonEmpty

data ListZipper a = ListZipper
    { before :: ![a]
    , current :: !a
    , after :: ![a]
    }
    deriving stock (Show, Eq, Ord, Functor)

next :: ListZipper a -> ListZipper a
next x@(ListZipper _ _ []) = x
next (ListZipper p c (n : ns)) = ListZipper (c : p) n ns

previous :: ListZipper a -> ListZipper a
previous x@(ListZipper [] _ _) = x
previous (ListZipper (p : ps) c n) = ListZipper ps p (c : n)

current :: ListZipper a -> a
current ListZipper{current} = current

swapCurrent :: a -> ListZipper a -> ListZipper a
swapCurrent x = modifyCurrent $ const x

modifyCurrent :: (a -> a) -> ListZipper a -> ListZipper a
modifyCurrent f l@ListZipper{current} = l{current = f current}

fromNonEmpty :: NonEmpty a -> ListZipper a
fromNonEmpty (x :| xs) = ListZipper [] x xs

type IsCurrent = Bool

renderZipper :: (IsCurrent -> a -> b) -> ListZipper a -> [b]
renderZipper f ListZipper{before, current, after} = fmap (f False) (List.reverse before) <> [f True current] <> fmap (f False) after
