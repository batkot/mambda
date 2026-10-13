module Mambda.Time (
  Clock,
  Time,
  Resolution(..),
  newClock,
  tick,
  schedule,
  scheduleTime,
  ticks,
  time,
  mkTime
) where

import Prelude

import Data.Time (NominalDiffTime)
import Data.Time qualified as Time
import Data.Fixed

data Clock = Clock
  { time :: Time
  , resolution :: Resolution
  }

newClock :: Resolution -> Clock
newClock = Clock $ Time 0

time :: Clock -> Time
time (Clock t _) = t

ticks :: Clock -> Integer
ticks Clock{time = Time t} = t

newtype Time = Time Integer
  deriving newtype (Eq, Ord, Show)

instance Semigroup Time where
  (<>) = addTime

instance Monoid Time where
  mempty = Time 0

addTime :: Time -> Time -> Time
addTime (Time a) (Time b) = Time $ a + b

mkTime :: Integral a => a -> Time
mkTime = Time . max 0 . toInteger

newtype Resolution = Resolution Integer

tick :: Time -> Clock -> Clock
tick a clock@Clock{time = b} = clock{time = addTime a b}

scheduleTime :: NominalDiffTime -> Clock -> Time
scheduleTime diff clock@(Clock _ (Resolution resolution)) = schedule interval clock
  where
    interval = Time $ toInteger $ fromEnum $ realToFrac @_ @(Fixed E0) $ fromInteger resolution * Time.nominalDiffTimeToSeconds diff

schedule :: Time -> Clock -> Time
schedule diff (Clock currTime _) = currTime `addTime` diff