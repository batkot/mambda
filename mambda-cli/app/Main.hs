module Main (main) where

import Prelude

import Brick qualified
import Graphics.Vty qualified as Vty

import Control.Monad (void)
import Data.List.NonEmpty
import Data.Text qualified as Text
import Mambda.Game qualified as Game
import Mambda.MainMenu qualified as MainMenu

data MambdaCliState
    = Menu (MainMenu.State MenuItem)
    | Game Game.State
    deriving stock (Show, Eq, Ord)

data MambdaCliResource = MambdaCliResource
    deriving stock (Show, Eq, Ord)

data MenuItem = MenuItem
    { label :: Text.Text
    , transitionTo :: MambdaCliState
    }
    deriving stock (Show, Eq, Ord)

instance MainMenu.MenuItem MenuItem where
    toMenuItem MenuItem{label} = label

mainMenu :: NonEmpty MenuItem
mainMenu =
    MenuItem{label = "Start Game", transitionTo = Game Game.initState}
        :| [ MenuItem{label = "Scoreboard", transitionTo = Game Game.initState}
           ]

main :: IO ()
main =
    void $ Brick.defaultMain app $ Menu $ MainMenu.initState mainMenu
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
                (newMenuState, proceed) <- Brick.nestEventM menuState (MainMenu.handleEvent ev)
                case proceed of
                    Nothing -> Brick.put $ Menu newMenuState
                    Just MenuItem{transitionTo} -> Brick.put transitionTo
            Game gameState -> do
                newGameState <- Brick.nestEventM' gameState (Game.handleEvent ev)
                Brick.put $ Game newGameState
    drawUI :: MambdaCliState -> [Brick.Widget MambdaCliResource]
    drawUI = \case
        Menu menuState -> [MainMenu.render menuState]
        Game gameState -> [Game.render gameState]
