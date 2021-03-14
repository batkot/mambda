{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Mambda.Rules
    ( test_rules

    -- tmp
    , RunningGame(..)
    , PausedGame(..)
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
step_PausedGameDoesntChange (PausedGame g) = g == stepSnakeEffect g

step_RunningGameMovesSnakeForward :: RunningGame (Sum Int) -> Bool
step_RunningGameMovesSnakeForward (RunningGame g) = 
    ((<>) (gameSnakeSpeed g) . getHead . gameSnake) g == (getHead . gameSnake . stepSnakeEffect) g

-- Arbitrary
instance Arbitrary GameStatus where
    arbitrary = elements [ Running, Paused, Finished ]

newtype PausedGame a = PausedGame { pausedGame :: Game a } deriving (Show, Eq)

instance (Monoid a, Arbitrary a) => Arbitrary (PausedGame a) where
    arbitrary = PausedGame . pauseGame . runningGame <$> arbitrary

newtype RunningGame a = RunningGame { runningGame :: Game a } deriving (Show,Eq)

instance (Monoid a, Arbitrary a) => Arbitrary (RunningGame a) where
    arbitrary = fmap RunningGame $ newGame <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Object a) where
    arbitrary = Object <$> arbitrary <*> pure id
