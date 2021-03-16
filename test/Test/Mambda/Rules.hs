{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Mambda.Rules
    ( test_rules

    -- tmp
    , RunningGame(..)
    , PausedGame(..)
    ) where

import Mambda.Rules
import Mambda.Snake

import Test.Mambda.Arbitrary

import Data.Monoid (Sum)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

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
