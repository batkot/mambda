{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Mambda.Rules
    ( test_rules
    ) where

import Mambda.Rules
import Mambda.Snake

import Test.Mambda.Snake -- to import arbitrary instances

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Test.QuickCheck (Arbitrary(..), elements, applyFun2, Fun, Function, Gen, CoArbitrary)

test_rules :: TestTree
test_rules = testGroup "Rules"
    [ testGroup "Game step" 
        [ testProperty "Paused game doesn't change" step_PausedGameDoesntChange
        , testProperty "Running game moves snake according to direction" step_RunningGameMovesSnakeForward
        ]
    ]

step_PausedGameDoesntChange :: PausedGame Int Int -> Bool
step_PausedGameDoesntChange (PausedGame g) = g == step g

step_RunningGameMovesSnakeForward :: RunningGame Int Int -> Bool
step_RunningGameMovesSnakeForward (RunningGame g@(Game inputSnake dir geometry _)) = 
    (moveFun dir . getHead ) inputSnake == (getHead . snake . step) g
  where
    moveFun = moveDir geometry

-- Arbitrary
--
instance (Function a, Function d, CoArbitrary a, CoArbitrary d, Arbitrary a, Arbitrary d) => Arbitrary (Game a d) where
    arbitrary = Game 
        <$> arbitrary 
        <*> arbitrary 
        <*> (Geometry . applyFun2 <$> arbitrary) 
        <*> arbitrary

newtype PausedGame a d = PausedGame { pausedGame :: Game a d } deriving (Show, Eq)

instance (Function a, Function d, CoArbitrary a, CoArbitrary d, Arbitrary a, Arbitrary d) => Arbitrary (PausedGame a d) where
    arbitrary = PausedGame . pause <$> arbitrary
      where
        pause game = game { pause = True }

newtype RunningGame a d = RunningGame { runningGame :: Game a d } deriving (Show,Eq)

instance (Function a, Function d, CoArbitrary a, CoArbitrary d, Arbitrary a, Arbitrary d) => Arbitrary (RunningGame a d) where
    arbitrary = RunningGame . pause <$> arbitrary
      where
        pause game = game { pause = False }
