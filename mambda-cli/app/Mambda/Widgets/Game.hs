module Mambda.Widgets.Game (
    State,
    initState,
    render,
    handleEvent,
    attributeMap,
) where

import Prelude

import Data.Functor.Identity

import Brick qualified
import Brick.Widgets.Center qualified as Brick
import Mambda.Game qualified as Game

import Brick.Widgets.Border qualified as Brick
import Data.Vector qualified as Vector
import Graphics.Vty qualified as Vty

data State = State
    { game :: Game.State Identity
    , finished :: Bool
    }

renderGlyph :: Game.Glyph -> Brick.Widget n
renderGlyph Game.Empty = Brick.withAttr emptyAttr $ Brick.str "  "
renderGlyph Game.Snake = Brick.withAttr snakeAttr $ Brick.str "██"
renderGlyph Game.SnakeSegment = Brick.withAttr snakeSegmentAttr $ Brick.str "██"
renderGlyph Game.Wall = Brick.withAttr wallAttr $ Brick.str "░░"
renderGlyph Game.Apple = Brick.withAttr appleAttr $ Brick.str "██"
renderGlyph Game.GoldenApple = Brick.withAttr goldenAppleAttr $ Brick.str "██"
renderGlyph Game.Portal = Brick.withAttr portalAttr $ Brick.str "▌▐"
renderGlyph Game.Poison = Brick.withAttr poisonAttr $ Brick.str "██"
renderGlyph Game.Laser = Brick.withAttr laserAttr $ Brick.str "╪╪"

initState :: State
initState = State (runIdentity $ Game.init $ Game.WorldSettings 20 20) False

handleEvent :: Brick.BrickEvent n e -> Brick.EventM n State ()
handleEvent (Brick.AppEvent _) = Brick.modify $ \(State g _) -> uncurry (flip State) $ runIdentity $ Game.step g
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KUp [])) =
    Brick.modify $ \s@(State g _) -> s{game = runIdentity $ Game.control Game.up g}
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KDown [])) =
    Brick.modify $ \s@(State g _) -> s{game = runIdentity $ Game.control Game.down g}
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KLeft [])) =
    Brick.modify $ \s@(State g _) -> s{game = runIdentity $ Game.control Game.left g}
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KRight [])) =
    Brick.modify $ \s@(State g _) -> s{game = runIdentity $ Game.control Game.right g}
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KEnter [])) =
    Brick.modify $ \s@(State _ r) -> if r then initState else s
handleEvent (Brick.VtyEvent (Vty.EvKey (Vty.KChar ' ') [])) =
    Brick.modify $ \s@(State g _) -> s{game = runIdentity $ Game.laser g}
handleEvent _ = pure ()

render :: State -> Brick.Widget n
render (State s finished) =
    Brick.center $
        Brick.vCenter $
            Brick.borderWithLabel (Brick.str label) frame
  where
    Game.Render r = runIdentity $ Game.render s
    label = if finished then "Game Over" else "Game"
    frame = Brick.vBox $ Vector.toList $ fmap renderRow r
    renderRow v = Brick.hBox $ Vector.toList $ renderGlyph <$> v

attributeMap :: Brick.AttrMap
attributeMap =
    Brick.attrMap
        Vty.defAttr
        [ (snakeAttr, Vty.brightGreen `Brick.on` Vty.brightGreen)
        , (snakeSegmentAttr, Vty.green `Brick.on` Vty.green)
        , (emptyAttr, Vty.defAttr)
        , (wallAttr, Vty.brightBlack `Brick.on` Vty.black)
        , (appleAttr, Vty.brightRed `Brick.on` Vty.brightRed)
        , (goldenAppleAttr, Vty.brightYellow `Brick.on` Vty.brightYellow)
        , (portalAttr, Brick.fg Vty.brightBlue)
        , (poisonAttr, Vty.brightMagenta `Brick.on` Vty.brightMagenta)
        , (laserAttr, Brick.fg Vty.brightRed)
        ]

snakeAttr :: Brick.AttrName
snakeAttr = Brick.attrName "snake"

snakeSegmentAttr :: Brick.AttrName
snakeSegmentAttr = Brick.attrName "snakeSegment"

emptyAttr :: Brick.AttrName
emptyAttr = Brick.attrName "empty"

wallAttr :: Brick.AttrName
wallAttr = Brick.attrName "wall"

appleAttr :: Brick.AttrName
appleAttr = Brick.attrName "apple"

portalAttr :: Brick.AttrName
portalAttr = Brick.attrName "portal"

poisonAttr :: Brick.AttrName
poisonAttr = Brick.attrName "poison"

laserAttr :: Brick.AttrName
laserAttr = Brick.attrName "laser"

goldenAppleAttr :: Brick.AttrName
goldenAppleAttr = Brick.attrName "golden-apple"
