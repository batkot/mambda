{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Mambda 
    ( test_tests
    )
    where

import Mambda 

import Data.List.NonEmpty as NE 

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Test.QuickCheck (Arbitrary(..), elements)

test_tests :: TestTree
test_tests = testGroup "Hasnakell" 
    [ changeDirectionTests
    , moveTests
    ]

moveTests :: TestTree
moveTests = testGroup "Move snake"
    [ testProperty "Tail of body should take place of previous element" move_snakesBodyInitShouldBecomeTail
    , testProperty "Direction should not change" move_directionShouldNotChange
    ]

move_snakesBodyInitShouldBecomeTail :: Snake -> Bool
move_snakesBodyInitShouldBecomeTail snake = 
    (NE.init . body) snake == (NE.tail . body) newSnake
  where
    newSnake = move snake

move_directionShouldNotChange :: Snake -> Bool
move_directionShouldNotChange snake = 
    direction snake == direction newSnake
  where
    newSnake = move snake

changeDirectionTests :: TestTree
changeDirectionTests = testGroup "Change snake direction"
    [ testProperty "Should change direction" changeDirection_ShouldChangeSnakeDirection
    , testProperty "Should not change body" changeDirection_ShouldNotChangeSnakeBody
    ]

changeDirection_ShouldChangeSnakeDirection :: Direction -> Snake -> Bool
changeDirection_ShouldChangeSnakeDirection dir snake = 
    dir == direction newSnake
  where
    newSnake = changeDirection dir snake

changeDirection_ShouldNotChangeSnakeBody :: Direction -> Snake -> Bool
changeDirection_ShouldNotChangeSnakeBody dir snake = 
    body snake == body newSnake
  where
    newSnake = changeDirection dir snake

-- Arbitrary
instance Arbitrary Direction where
    arbitrary = elements [North, South, East, West]

instance Arbitrary Snake where
    arbitrary = Snake <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Vec2 a) where
    arbitrary = Vec2 <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
