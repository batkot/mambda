module Mambda.Game where

import Prelude

import Aztecs.ECS qualified as Aztecs

newtype Position a = Position a
  deriving stock (Show)
  deriving anyclass (Aztecs.Component m)

newtype Velocity a = Velocity a
  deriving stock (Show)
  deriving anyclass (Aztecs.Component m)

type Ticks = Word

newtype Lifetime = Lifetime Ticks
  deriving stock (Show)
  deriving anyclass (Aztecs.Component m)

