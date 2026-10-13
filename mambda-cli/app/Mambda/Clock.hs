module Mambda.Clock where

import Data.Time

class WithClock a where
  timeElapsed :: a -> NominalDiffTime