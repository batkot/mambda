module Test.Mambda.Arbitrary
    where

import Mambda.Utils (PositiveInt, createPositiveInt, getInt )
import Mambda.Snake
import Mambda.Rules
import Mambda.Objects

import Data.Maybe (fromJust)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty(..))

import Test.QuickCheck (Arbitrary(..), elements, listOf1)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

test_arbitrary :: TestTree
test_arbitrary = testGroup "Arbitrary smoke tests"
    [ testGroup "PositiveInt" 
        [ testProperty "Should always create positive int" $ (<) 0 . getInt ]

    , testGroup "Snakes" 
        [ testProperty "Grow count is not negative" snakeArbitrary_growCountShouldNotBeNegative 
        , testProperty "Non growing snake grow count should be 0" nonGrowingSnakeArbitrary_growCountShouldBeZero
        , testProperty "Growing snake should have positive grow count" growingSnakeArbitrary_growCountShouldBePositive
        ]
    ]

-- PositiveInt
instance Arbitrary PositiveInt where
    arbitrary = fromJust . createPositiveInt . (+1) . abs <$> arbitrary
    
-- Snake
type SnakeType = Int

instance Arbitrary a => Arbitrary (Snake a) where
    arbitrary = Snake <$> fmap abs arbitrary <*> arbitrary

snakeArbitrary_growCountShouldNotBeNegative :: Snake SnakeType -> Bool
snakeArbitrary_growCountShouldNotBeNegative = (<=) 0 . growCount

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary

newtype NonGrowingSnake a = NonGrowingSnake { getNonGrowingSnake :: Snake a } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (NonGrowingSnake a) where
    arbitrary = NonGrowingSnake . Snake 0 <$> arbitrary

nonGrowingSnakeArbitrary_growCountShouldBeZero :: NonGrowingSnake SnakeType -> Bool
nonGrowingSnakeArbitrary_growCountShouldBeZero = (==) 0 . growCount . getNonGrowingSnake

newtype GrowingSnake a = GrowingSnake { getGrowingSnake :: Snake a } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (GrowingSnake a) where
    arbitrary = GrowingSnake <$> fmap increaseGrow arbitrary
      where
        increaseGrow (Snake g x) = Snake (g+1) x

growingSnakeArbitrary_growCountShouldBePositive :: GrowingSnake SnakeType -> Bool
growingSnakeArbitrary_growCountShouldBePositive = (<) 0 . growCount . getGrowingSnake


-- Games
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

-- Objects
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
