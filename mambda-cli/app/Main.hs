module Main (main) where

import Prelude

import Brick qualified
import Graphics.Vty qualified as Vty

import Control.Monad (void)
import Mambda.MainMenu qualified as MainMenu

newtype MambdaCliState
    = Menu MainMenu.State
    deriving stock (Show, Eq, Ord)

data MambdaCliResource = MambdaCliResource
    deriving stock (Show, Eq, Ord)

main :: IO ()
main =
    void $ Brick.defaultMain app $ Menu MainMenu.initState
  where
    app :: Brick.App MambdaCliState () MambdaCliResource
    app =
        Brick.App
            { appDraw = drawUI
            , appChooseCursor = \_ _ -> Nothing
            , appHandleEvent = handleEvent
            , appStartEvent = pure ()
            , appAttrMap = const $ Brick.attrMap Vty.defAttr []
            }
    handleEvent :: Brick.BrickEvent MambdaCliResource () -> Brick.EventM MambdaCliResource MambdaCliState ()
    handleEvent (Brick.VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = Brick.halt
    handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KEsc [])) = Brick.halt
    handleEvent ev = do
        state <- Brick.get
        case state of
            Menu menuState -> do
                newMenuState <- Brick.nestEventM' menuState (MainMenu.handleEvent ev)
                Brick.put $ Menu newMenuState
    drawUI :: MambdaCliState -> [Brick.Widget MambdaCliResource]
    drawUI = \case
        Menu menuState -> [MainMenu.render menuState]
