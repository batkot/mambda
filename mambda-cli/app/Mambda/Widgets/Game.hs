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

newtype State = State (Game.State Identity)

renderGlyph :: Game.Glyph -> Brick.Widget n
renderGlyph Game.Empty = Brick.withAttr emptyAttr $ Brick.str "██"
renderGlyph Game.Snake = Brick.withAttr snakeAttr $ Brick.str "██"
renderGlyph Game.SnakeSegment = Brick.withAttr snakeSegmentAttr $ Brick.str "██"
renderGlyph Game.Wall = Brick.withAttr wallAttr $ Brick.str "██"

initState :: State
initState = State $ runIdentity Game.init

handleEvent :: Brick.BrickEvent n e -> Brick.EventM n State ()
handleEvent (Brick.AppEvent _) = Brick.modify $ \(State g) -> State $ runIdentity $ Game.step g
handleEvent _ = pure ()

render :: State -> Brick.Widget n
render (State s) =
    Brick.center $
        Brick.vCenter $
            Brick.borderWithLabel (Brick.str "Game") frame
  where
    Game.Render r = runIdentity $ Game.render s
    frame = Brick.vBox $ Vector.toList $ fmap renderRow r
    renderRow v = Brick.hBox $ Vector.toList $ renderGlyph <$> v

-- emptyGrid :: Map Integer (Map Integer Glyph)
-- emptyGrid = Map.fromList $ do
--     x <- [1 .. 10]
--     pure $ (x, Map.fromList $ [(y, Empty) | y <- [1 .. 10]])
-- renderGrid :: Map Integer (Map Integer Glyph) -> Brick.Widget n
-- renderGrid m =
--     let rows = Map.elems m
--         renderCol map' = Brick.hBox $ renderGlyph <$> Map.elems map'
--      in Brick.vBox $ renderCol <$> rows
-- foo = runIdentity $ Game.render s
-- updateElement :: Glyph -> Integer -> Integer -> Map Integer (Map Integer Glyph) -> Map Integer (Map Integer Glyph)
-- updateElement glyph x y = Map.update (Just . Map.insert y glyph) x
-- frameGrid = Vector.foldl' (\m (Game.Position (V2 x y)) -> updateElement SnakeHead x y m) emptyGrid foo

attributeMap :: Brick.AttrMap
attributeMap =
    Brick.attrMap
        Vty.defAttr
        [ (snakeAttr, Vty.brightGreen `Brick.on` Vty.brightGreen)
        , (snakeSegmentAttr, Vty.green `Brick.on` Vty.green)
        , (emptyAttr, Vty.black `Brick.on` Vty.black)
        , (wallAttr, Vty.brightBlack `Brick.on` Vty.brightBlack)
        ]

snakeAttr :: Brick.AttrName
snakeAttr = Brick.attrName "snake"

snakeSegmentAttr :: Brick.AttrName
snakeSegmentAttr = Brick.attrName "snakeSegment"

emptyAttr :: Brick.AttrName
emptyAttr = Brick.attrName "empty"

wallAttr :: Brick.AttrName
wallAttr = Brick.attrName "wall"
