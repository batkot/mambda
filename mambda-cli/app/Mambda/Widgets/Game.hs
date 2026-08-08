module Mambda.Widgets.Game (
    State,
    initState,
    render,
    handleEvent,
    attributeMap,
) where

import Prelude

import Data.Functor.Identity

import Aztecs qualified

import Data.Map (Map)
import Data.Map qualified as Map

import Brick qualified
import Brick.Widgets.Center qualified as Brick
import Mambda.Game qualified as Game

import Brick.Widgets.Border qualified as Brick
import Data.Vector qualified as Vector
import Graphics.Vty qualified as Vty
import Linear

newtype State = State (Game.State Identity)

data Glyph = SnakeHead | Empty

renderGlyph :: Glyph -> Brick.Widget n
renderGlyph Empty = Brick.withAttr emptyAttr $ Brick.str "██"
renderGlyph SnakeHead = Brick.withAttr snakeAttr $ Brick.str "██"

initState :: State
initState = State $ runIdentity Game.initWorld

handleEvent :: Brick.BrickEvent n e -> Brick.EventM n State ()
handleEvent (Brick.AppEvent _) = Brick.modify $ \(State g) -> State $ runIdentity $ Game.step g
handleEvent _ = pure ()

render :: State -> Brick.Widget n
render (State (Game.State world)) =
    Brick.center $
        Brick.vCenter $
            Brick.borderWithLabel (Brick.str "Game") $
                renderGrid frameGrid
  where
    emptyGrid :: Map Integer (Map Integer Glyph)
    emptyGrid = Map.fromList $ do
        x <- [1 .. 10]
        pure $ (x, Map.fromList $ [(y, Empty) | y <- [1 .. 10]])
    renderGrid :: Map Integer (Map Integer Glyph) -> Brick.Widget n
    renderGrid m =
        let rows = Map.elems m
            renderCol map' = Brick.hBox $ renderGlyph <$> Map.elems map'
         in Brick.vBox $ renderCol <$> rows
    foo = fst $ runIdentity $ Aztecs.runAccess Game.foo world
    updateElement :: Glyph -> Integer -> Integer -> Map Integer (Map Integer Glyph) -> Map Integer (Map Integer Glyph)
    updateElement glyph x y = Map.update (Just . Map.insert y glyph) x
    frameGrid = Vector.foldl' (\m (Game.Position (V2 x y)) -> updateElement SnakeHead x y m) emptyGrid foo

attributeMap :: Brick.AttrMap
attributeMap =
    Brick.attrMap
        Vty.defAttr
        [ (snakeAttr, Vty.green `Brick.on` Vty.green)
        , (emptyAttr, Vty.black `Brick.on` Vty.black)
        ]

snakeAttr :: Brick.AttrName
snakeAttr = Brick.attrName "snake"

emptyAttr :: Brick.AttrName
emptyAttr = Brick.attrName "empty"
