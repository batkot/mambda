{-# LANGUAGE LambdaCase #-}
module Test.Mambda.Flatland
    -- (test_flatland)
    where

import Mambda
import Mambda.Flatland
import Mambda.Utils

import Test.Mambda.Utils

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Test.QuickCheck (Arbitrary(..), elements)

-- Good tests for world map

-- test_flatland :: TestTree
-- test_flatland = testGroup "Flatland" 
--     [ testGroup "Geometry" 
--         [ testProperty "Cords should stay in given modulo space" geometry_coordsShouldAlwaysEndInModuloSpace
--         , testProperty "Move into opposite direction should get you back where you started" geometry_directionsAreInverseElementsForMove
--         , testProperty "Move is commutative" geometry_moveCommutative
--         ]
--     ]

-- geometry_coordsShouldAlwaysEndInModuloSpace :: PositiveInt -> PositiveInt -> Direction2D -> Vec2D -> Bool
-- geometry_coordsShouldAlwaysEndInModuloSpace maxX maxY dir a =
--     x' < getInt maxX && x' >= 0 && y' < getInt maxY && y' >= 0
--   where
--     geometry = createModulusFlatlandGeometry maxX maxY
--     (x', y') = moveDir geometry dir a

-- geometry_directionsAreInverseElementsForMove :: PositiveInt -> PositiveInt -> Direction2D -> Vec2D -> Bool
-- geometry_directionsAreInverseElementsForMove maxX maxY dir (x,y) =
--     x' == (move oppositeDirection . move dir) x'
--   where
--     move = moveDir $ createModulusFlatlandGeometry maxX maxY
--     -- sanitize to be in bounds
--     x' = (x `mod` getInt maxX, y `mod` getInt maxY)
--     oppositeDirection = case dir of 
--         North -> South
--         South -> North
--         West -> East
--         East -> West

-- geometry_moveCommutative :: PositiveInt -> PositiveInt -> Direction2D -> Direction2D -> Vec2D -> Bool
-- geometry_moveCommutative maxX maxY dir1 dir2 x =
--     (move dir1 . move dir2) x == (move dir2 . move dir1) x
--   where
--     move = moveDir $ createModulusFlatlandGeometry maxX maxY

-- instance Arbitrary Direction2D where
--     arbitrary = elements [North, South, West, East]
