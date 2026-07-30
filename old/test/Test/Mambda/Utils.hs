{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Mambda.Utils
    ( test_utils )
    where

import Mambda.Utils
import Test.Mambda.Arbitrary

import Control.Arrow ((&&&))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

test_utils :: TestTree
test_utils = testGroup "Utils"
    [ testGroup "PositiveInt" 
        [ testProperty "Given positive int should create instance" $ uncurry expect . (id &&& Just) . (+1) . abs
        , testProperty "Given non positive int should return Nothing" $ uncurry expect . (id &&& const Nothing) . negate . abs
        ]
    ]

expect :: Int -> Maybe Int -> Bool
expect given expected =
    expected == positiveInt
  where
    positiveInt = getInt <$> createPositiveInt given
