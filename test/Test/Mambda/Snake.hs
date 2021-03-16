{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Mambda.Snake
    ( test_snake
    )
    where

import Mambda.Snake

import Data.List.NonEmpty as NE

import Test.Mambda.Arbitrary

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

type TestSnakeCell = Int

test_snake :: TestTree
test_snake = testGroup "Snake"
    [ moveTests
    ]

moveTests :: TestTree
moveTests = testGroup "Move snake"
    [ testProperty "Head should be moved to given place" move_snakesHeadShouldMove
    , testProperty "Grow count should be non negative number" move_growCountShouldBeNonNegative
    , testGroup "Non growing move"
        [ testProperty "Tail of body should take place of previous elements" move_snakesBodyInitShouldBecomeTail
        , testProperty "Size of snake should remain constant" move_snakeBodyShouldStayTheSameSize
        , testProperty "Grow count should remain 0" move_snakeGrowCountShouldRemainZero
        ]
    , testGroup "Growing move"
        [ testProperty "Snake should grow in size" move_growingSnakeShouldIncreaseInSize
        , testProperty "Snake grow count should decrease but not less then zero" move_growingSnakeGrowFactorShouldDecrease
        , testProperty "Old snakes whole body should become new snakes tail" move_growingSnakeTailShouldbeWholeOldSnake
        ]
    ]

type SnakeMoveProperty snake = TestSnakeCell -> snake TestSnakeCell -> Bool

move_snakesHeadShouldMove :: SnakeMoveProperty Snake
move_snakesHeadShouldMove newHead snake = 
    newHead == (NE.head . body . move newHead) snake

move_growCountShouldBeNonNegative :: SnakeMoveProperty Snake
move_growCountShouldBeNonNegative newHead snake = 
    (>=0) . growCount . move newHead $ snake

move_snakesBodyInitShouldBecomeTail :: SnakeMoveProperty NonGrowingSnake
move_snakesBodyInitShouldBecomeTail newHead (NonGrowingSnake snake) = 
    (NE.init . body) snake == (NE.tail . body . move newHead) snake

move_snakeBodyShouldStayTheSameSize :: SnakeMoveProperty NonGrowingSnake
move_snakeBodyShouldStayTheSameSize newHead (NonGrowingSnake snake) =
    (NE.length . body) snake == (NE.length . body . move newHead) snake

move_snakeGrowCountShouldRemainZero :: SnakeMoveProperty NonGrowingSnake
move_snakeGrowCountShouldRemainZero newHead (NonGrowingSnake snake) = 
    (==) 0 . growCount . move newHead $ snake

move_growingSnakeShouldIncreaseInSize :: SnakeMoveProperty GrowingSnake
move_growingSnakeShouldIncreaseInSize newHead (GrowingSnake snake) = 
    ((+1) . NE.length . body) snake == (NE.length . body . move newHead) snake

move_growingSnakeGrowFactorShouldDecrease :: SnakeMoveProperty GrowingSnake
move_growingSnakeGrowFactorShouldDecrease newHead (GrowingSnake snake) =
    growCount snake == ((+1) . growCount . move newHead) snake

move_growingSnakeTailShouldbeWholeOldSnake :: SnakeMoveProperty GrowingSnake
move_growingSnakeTailShouldbeWholeOldSnake newHead (GrowingSnake snake) =
    (NE.toList . body) snake == (NE.tail . body . move newHead) snake
