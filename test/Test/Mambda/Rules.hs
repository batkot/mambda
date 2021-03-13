{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Mambda.Rules
    ( test_rules
    ) where

import Mambda.Rules
import Mambda.Snake

import Test.Mambda.Snake -- to import arbitrary instances

import Data.Monoid (Sum)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Test.QuickCheck (Arbitrary(..), elements)

test_rules :: TestTree
test_rules = testGroup "Rules"
    [ testGroup "Game step" 
        [ testProperty "Paused game doesn't change" step_PausedGameDoesntChange
        , testProperty "Running game moves snake according to direction" step_RunningGameMovesSnakeForward
        ]
    ]

step_PausedGameDoesntChange :: PausedGame (Sum Int) -> Bool
step_PausedGameDoesntChange (PausedGame g) = g == step g

step_RunningGameMovesSnakeForward :: RunningGame (Sum Int) -> Bool
step_RunningGameMovesSnakeForward (RunningGame g@(Game inputSnake speed _ _ _ )) = 
    ((<>) speed . getHead ) inputSnake == (getHead . snake . step) g

-- Arbitrary
instance (Monoid a, Arbitrary a) => Arbitrary (Game a) where
    arbitrary = Game 
        <$> arbitrary 
        <*> arbitrary 
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary GameStatus where
    arbitrary = elements [ Running, Paused, Finished ]

newtype PausedGame a = PausedGame { pausedGame :: Game a } deriving (Show, Eq)

instance (Monoid a, Arbitrary a) => Arbitrary (PausedGame a) where
    arbitrary = PausedGame . pause <$> arbitrary
      where
        pause game = game { status = Paused }

newtype RunningGame a = RunningGame { runningGame :: Game a } deriving (Show,Eq)

instance (Monoid a, Arbitrary a) => Arbitrary (RunningGame a) where
    arbitrary = RunningGame . makeRunning <$> arbitrary
      where
        makeRunning game = game { status = Running }

instance Arbitrary a => Arbitrary (Object a) where
    arbitrary = Object <$> arbitrary <*> pure id
