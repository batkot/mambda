module Mambda.Game (
    State,
    initState,
    render,
    handleEvent,
) where

import Prelude

import Brick qualified
import Brick.Widgets.Center qualified as Brick

data State = State
    deriving stock (Show, Eq, Ord)

initState :: State
initState = State

handleEvent :: Brick.BrickEvent n () -> Brick.EventM n State ()
handleEvent _ = pure ()

render :: State -> Brick.Widget n
render _ =
    Brick.center $
        Brick.vCenter $
            Brick.str "Gameee"
