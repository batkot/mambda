module Test.Mambda.Objects 
    ( test_objects )
    where

import Mambda.Objects
import Mambda.Rules

import Test.Mambda.Arbitrary

import Data.Monoid (Sum)
import Data.List (nub)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

test_objects :: TestTree
test_objects = testGroup "Objects tests" 
    [ testGroup "Food tests"
        [ testProperty "Should increase score" food_shouldIncreaseScore

        , testGroup "No more food" 
            [ testProperty "When no more food, game should finish" food_noMoreFoodFinishesGame ]

        , testGroup "There's more food"
            [ testProperty "When not last food, should keep game going" food_notLastFoodShouldKeepGameGoing
            , testProperty "Number of object should be const" food_notLastNumberOfObjectsShouldBeConst
            ]
        ]
    ]

food_noMoreFoodFinishesGame :: RunningGame (Sum Int) -> LastFood (Sum Int) -> Bool
food_noMoreFoodFinishesGame (RunningGame game) (LastFood food) = 
    gameStatus (collision food game) == Finished

food_shouldIncreaseScore :: RunningGame (Sum Int) -> Food (Sum Int) -> Bool
food_shouldIncreaseScore (RunningGame game) (Food food) = 
    gameScore (collision food game) == ((+1) . gameScore) game

food_notLastFoodShouldKeepGameGoing :: RunningGame (Sum Int) -> NotLastFood (Sum Int) -> Bool
food_notLastFoodShouldKeepGameGoing (RunningGame game) (NotLastFood food _) = 
    gameStatus (collision food game) == Running

food_notLastNumberOfObjectsShouldBeConst :: RunningGame (Sum Int) -> NotLastFood (Sum Int) -> Bool
food_notLastNumberOfObjectsShouldBeConst (RunningGame game) (NotLastFood food _) = 
    (length . gameObjects . collision food) game == (length . gameObjects) game
