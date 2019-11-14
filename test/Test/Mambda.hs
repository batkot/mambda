{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Mambda
    ( test_tests
    )
    where

import Mambda

import Data.List.NonEmpty as NE
import Data.Semigroup

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Test.QuickCheck (Arbitrary(..), elements)

type TestSemigroup = Sum Int

test_tests :: TestTree
test_tests = testGroup "Hasnakell"
    [ changeDirectionTests
    , moveTests
    , growTests
    ]

moveTests :: TestTree
moveTests = testGroup "Move snake"
    [ testProperty "Tail of body should take place of previous elements" move_snakesBodyInitShouldBecomeTail
    , testProperty "Head should move according to current velocity" move_snakeHeadShouldMoveAccordingToSemigroup
    , testProperty "Direction should not change" move_velocityShouldNotChange
    ]

move_snakesBodyInitShouldBecomeTail :: Snake TestSemigroup -> Bool
move_snakesBodyInitShouldBecomeTail snake =
    (NE.init . body) snake == (NE.tail . body) newSnake
  where
    newSnake = move snake

move_snakeHeadShouldMoveAccordingToSemigroup :: Snake TestSemigroup -> Bool
move_snakeHeadShouldMoveAccordingToSemigroup snake =
    velocity snake <> (NE.head . body) snake == (NE.head . body) newSnake
  where
    newSnake = move snake

move_velocityShouldNotChange :: Snake TestSemigroup -> Bool
move_velocityShouldNotChange snake =
    velocity snake == velocity newSnake
  where
    newSnake = move snake

growTests :: TestTree
growTests = testGroup "Grow snake"
    [ testProperty "Snake should be 1 piece longer" grow_snakeShouldGrow
    , testProperty "Tail of new snake should be whole old snake body" grow_newSnakeTailShouldBeOldSnakesBody
    , testProperty "Head should move according to current velocity" grow_headShouldMoveAccordingToSemigroup
    , testProperty "Velocity should not change" grow_velocityShouldNotChange
    ]

grow_snakeShouldGrow :: Snake TestSemigroup -> Bool
grow_snakeShouldGrow snake = 
    ((+1) . NE.length . body) snake == (NE.length . body) newSnake
  where
    newSnake = grow snake

grow_newSnakeTailShouldBeOldSnakesBody :: Snake TestSemigroup -> Bool
grow_newSnakeTailShouldBeOldSnakesBody snake = 
    (NE.toList . body) snake == (NE.tail . body) newSnake
  where
    newSnake = grow snake

grow_headShouldMoveAccordingToSemigroup :: Snake TestSemigroup -> Bool
grow_headShouldMoveAccordingToSemigroup snake = 
    velocity snake <> (NE.head . body) snake == (NE.head . body) newSnake
  where
    newSnake = grow snake

grow_velocityShouldNotChange :: Snake TestSemigroup -> Bool
grow_velocityShouldNotChange snake =
    velocity snake == velocity newSnake
  where
    newSnake = grow snake

changeDirectionTests :: TestTree
changeDirectionTests = testGroup "Change snake velocity"
    [ testProperty "Should change velocity" changeDirection_ShouldChangeSnakeDirection
    , testProperty "Should not change body" changeDirection_ShouldNotChangeSnakeBody
    ]

changeDirection_ShouldChangeSnakeDirection :: TestSemigroup -> Snake TestSemigroup -> Bool
changeDirection_ShouldChangeSnakeDirection dir snake =
    dir == velocity newSnake
  where
    newSnake = changeDirection dir snake

changeDirection_ShouldNotChangeSnakeBody :: TestSemigroup -> Snake TestSemigroup -> Bool
changeDirection_ShouldNotChangeSnakeBody dir snake =
    body snake == body newSnake
  where
    newSnake = changeDirection dir snake

-- Arbitrary
instance Arbitrary a => Arbitrary (Snake a) where
    arbitrary = Snake <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
