module Test.Mambda.Objects 
    ( test_objects )
    where

import Mambda.Objects
import Mambda.Rules

import Test.Mambda.Rules -- to import arbitrary instances
import Test.Mambda.Utils

import Data.Monoid (Sum)
import Data.List (nub)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Test.QuickCheck (Arbitrary(..), elements, listOf1)

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

newtype LastFood a = LastFood (Object a) deriving (Eq, Show)
newtype Food a = Food (Object a) deriving (Eq, Show)
data NotLastFood a = NotLastFood 
    { currentFood :: (Object a)
    , nextFoodLoc :: a 
    } deriving (Eq, Show)

instance (Arbitrary a, GameTile a) => Arbitrary (LastFood a) where
    arbitrary = fmap LastFood $ food <$> arbitrary <*> arbitrary <*> pure []

instance (Arbitrary a, GameTile a) => Arbitrary (Food a) where
    arbitrary = fmap Food $ food <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, GameTile a) => Arbitrary (NotLastFood a) where
    arbitrary = do 
        next <- nub <$> listOf1 arbitrary
        current <- food <$> arbitrary <*> arbitrary <*> pure next
        return $ NotLastFood current (head next)
