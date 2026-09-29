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
import GHC.Generics
import Lens.Micro
import Mambda.Widgets.Game qualified as Game
import Mambda.Widgets.MainMenu qualified as MainMenu
import Mambda.Widgets.Settings qualified as Settings

import Data.List.NonEmpty qualified as NonEmpty
import Mambda.Game qualified as Game (PlayerId (..))

data MambdaCliState = MambdaCliState
    { currentScreen :: MambdaScreen
    , screenHistory :: [MambdaScreen]
    }
    deriving stock (Generic)

data MambdaScreen
    = Menu (MainMenu.State MenuItem)
    | Game Game.State
    | Settings (Settings.State MambdaCliResource MambdaEvent)

data MambdaCliResource = MambdaCliResource
    deriving stock (Show, Eq, Ord)

data MenuItem = MenuItem
    { label :: Text.Text
    , transitionTo :: MambdaScreen
    }

instance MainMenu.MenuItem MenuItem where
    toMenuItem MenuItem{label} = label

data MambdaEvent = Tick

mainMenu :: Settings.Settings -> NonEmpty MenuItem
mainMenu settings =
    MenuItem{label = "Classic", transitionTo = Game $ Game.initState (NonEmpty.singleton Game.One) settings}
        :| [ MenuItem{label = "Versus Mode", transitionTo = Game $ Game.initState (Game.One :| [Game.Two]) settings}
           , MenuItem{label = "Settings", transitionTo = Settings $ Settings.initState settings}
           ]

main :: IO ()
main = do
    tickChan <- BChan.newBChan 10
    void $ Concurrent.forkIO $ forever $ do
        BChan.writeBChan tickChan Tick
        Concurrent.threadDelay 250_000
    initVty <- buildVty
    void $ Brick.customMain initVty buildVty (Just tickChan) app $ MambdaCliState (Menu $ MainMenu.initState $ mainMenu Settings.defaultSettings) mempty
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

    appAttrMap MambdaCliState{currentScreen} = case currentScreen of
        Menu _ -> MainMenu.attributeMap
        Game _ -> Game.attributeMap
        Settings _ -> Settings.attributeMap
    handleEvent :: Brick.BrickEvent MambdaCliResource MambdaEvent -> Brick.EventM MambdaCliResource MambdaCliState ()
    handleEvent (Brick.VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = Brick.halt
    handleEvent (Brick.VtyEvent (Vty.EvKey Vty.KEsc [])) = do
        MambdaCliState{screenHistory} <- Brick.get
        case screenHistory of
            [] -> Brick.halt
            x : xs -> Brick.put $ MambdaCliState x xs
    handleEvent ev = do
        MambdaCliState{currentScreen, screenHistory} <- Brick.get
        case currentScreen of
            Menu menuState -> do
                (newMenuState, proceed) <- Brick.nestEventM menuState (MainMenu.handleEvent ev)
                case proceed of
                    Nothing -> Brick.modify $ #currentScreen .~ Menu newMenuState
                    Just MenuItem{transitionTo} -> Brick.put $ MambdaCliState transitionTo (currentScreen : screenHistory)
            Game gameState -> do
                newGameState <- Brick.nestEventM' gameState (Game.handleEvent ev)
                Brick.modify $ #currentScreen .~ Game newGameState
            Settings settingsState -> do
                (newSettingsState, done) <- Brick.nestEventM settingsState (Settings.handleEvent ev)
                case done of
                    Just settings -> Brick.put $ MambdaCliState (Menu . MainMenu.initState . mainMenu $ settings) (currentScreen : screenHistory)
                    Nothing -> Brick.modify $ #currentScreen .~ Settings newSettingsState

    drawUI :: MambdaCliState -> [Brick.Widget MambdaCliResource]
    drawUI MambdaCliState{currentScreen} = case currentScreen of
        Menu menuState -> [MainMenu.render menuState]
        Game gameState -> [Game.render gameState]
        Settings settingsState -> [Settings.render settingsState]
