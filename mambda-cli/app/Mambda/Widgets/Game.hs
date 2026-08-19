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

import Brick ((<+>), (<=>))
import Brick.Widgets.Border qualified as Brick
import Brick.Widgets.Table qualified as Table
import Control.Monad (unless)
import Data.Bifunctor
import Data.Vector qualified as Vector
import Graphics.Vty qualified as Vty

data GameState = Running | Paused | Finished
    deriving stock (Eq)

data State = State
    { game :: Game.State Identity
    , state :: GameState
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
initState = State (runIdentity $ Game.init $ Game.WorldSettings 20 20) Running

boolToState :: Bool -> GameState
boolToState True = Finished
boolToState False = Running

handleEvent :: Brick.BrickEvent n e -> Brick.EventM n State ()
handleEvent (Brick.AppEvent _) = do
    paused <- Brick.gets $ \(State{state}) -> state == Paused
    unless paused $ Brick.modify $ \(State g s) -> uncurry (flip State) $ runIdentity $ first boolToState <$> Game.step g
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KUp [])) =
    Brick.modify $ \s@(State g _) -> s{game = runIdentity $ Game.control (Game.ChangeDirection Game.up) g}
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KDown [])) =
    Brick.modify $ \s@(State g _) -> s{game = runIdentity $ Game.control (Game.ChangeDirection Game.down) g}
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KLeft [])) =
    Brick.modify $ \s@(State g _) -> s{game = runIdentity $ Game.control (Game.ChangeDirection Game.left) g}
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KRight [])) =
    Brick.modify $ \s@(State g _) -> s{game = runIdentity $ Game.control (Game.ChangeDirection Game.right) g}
handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KEnter [])) =
    Brick.modify $ \s@(State _ r) -> if r == Finished then initState else s
handleEvent (Brick.VtyEvent (Vty.EvKey (Vty.KChar 'p') [])) = do
    gameState <- Brick.gets $ \(State{state}) -> state
    case gameState of
        Finished -> pure ()
        Paused -> Brick.modify $ \s -> s{state = Running}
        Running -> Brick.modify $ \s -> s{state = Paused}
handleEvent (Brick.VtyEvent (Vty.EvKey (Vty.KChar ' ') [])) =
    Brick.modify $ \s@(State g _) -> s{game = runIdentity $ Game.laser g}
handleEvent _ = pure ()

render :: State -> Brick.Widget n
render (State s status) =
    Brick.center $
        Brick.vCenter $
            Brick.border (Table.renderTable frameTable)
                <=> Table.renderTable (borderlessTable $ Table.table [[Brick.str "SCORE", statusWidget]])
  where
    Game.Render r = runIdentity $ Game.render s
    statusWidget = Brick.str $ case status of
        Finished -> "GAME OVER"
        Paused -> "PAUSED"
        Running -> ""
    frameTable = borderlessTable $ Table.table worldGrid
    borderlessTable = Table.columnBorders False . Table.rowBorders False . Table.surroundingBorder False
    worldGrid = Vector.toList $ Vector.toList . fmap renderGlyph <$> r

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
