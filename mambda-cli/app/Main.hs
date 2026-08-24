module Main (main) where

import Prelude

import Brick qualified
import Brick.BChan qualified as BChan
import Graphics.Vty qualified as Vty
import Graphics.Vty.CrossPlatform as VtyX

import Control.Concurrent qualified as Concurrent
import Control.Monad (forever, void)
import Data.List.NonEmpty
import Data.Text qualified as Text
import Mambda.Widgets.Game qualified as Game
import Mambda.Widgets.MainMenu qualified as MainMenu
import Mambda.Widgets.Settings qualified as Settings

data MambdaCliState
    = Menu (MainMenu.State MenuItem)
    | Game Game.State
    | Settings (Settings.State MambdaCliResource MambdaEvent)

data MambdaCliResource = MambdaCliResource
    deriving stock (Show, Eq, Ord)

data MenuItem = MenuItem
    { label :: Text.Text
    , transitionTo :: MambdaCliState
    }

instance MainMenu.MenuItem MenuItem where
    toMenuItem MenuItem{label} = label

data MambdaEvent = Tick

mainMenu :: Settings.Settings -> NonEmpty MenuItem
mainMenu settings =
    MenuItem{label = "Start Game", transitionTo = Game $ Game.initState settings}
        :| [ MenuItem{label = "Settings", transitionTo = Settings Settings.initState}
           ]

main :: IO ()
main = do
    tickChan <- BChan.newBChan 10
    void $ Concurrent.forkIO $ forever $ do
        BChan.writeBChan tickChan Tick
        Concurrent.threadDelay 250_000
    initVty <- buildVty
    void $ Brick.customMain initVty buildVty (Just tickChan) app $ Menu $ MainMenu.initState $ mainMenu Settings.defaultSettings
  where
    buildVty = VtyX.mkVty Vty.defaultConfig
    app :: Brick.App MambdaCliState MambdaEvent MambdaCliResource
    app =
        Brick.App
            { appDraw = drawUI
            , appChooseCursor = \_ _ -> Nothing
            , appHandleEvent = handleEvent
            , appStartEvent = pure ()
            , appAttrMap = appAttrMap
            }

    appAttrMap = \case
        Menu _ -> MainMenu.attributeMap
        Game _ -> Game.attributeMap
        Settings _ -> Settings.attributeMap
    handleEvent :: Brick.BrickEvent MambdaCliResource MambdaEvent -> Brick.EventM MambdaCliResource MambdaCliState ()
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
            Settings settingsState -> do
                (newSettingsState, done) <- Brick.nestEventM settingsState (Settings.handleEvent ev)
                let newState = maybe (Settings newSettingsState) (Menu . MainMenu.initState . mainMenu) done
                Brick.put newState

    drawUI :: MambdaCliState -> [Brick.Widget MambdaCliResource]
    drawUI = \case
        Menu menuState -> [MainMenu.render menuState]
        Game gameState -> [Game.render gameState]
        Settings settingsState -> [Settings.render settingsState]
