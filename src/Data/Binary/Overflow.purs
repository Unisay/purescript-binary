module Data.Binary.Overflow
  ( Overflow(..)
  , discardOverflow
  ) where

import Prelude

data Overflow b a = Overflow b a

derive instance eqOverflow :: (Eq b, Eq a) => Eq (Overflow b a)

instance showOverflow :: (Show b, Show a) => Show (Overflow b a) where
  show (Overflow b a) = show a <> " with " <> show b <> " overflow"

instance functorOverflow :: Functor (Overflow b) where
  map f (Overflow o a) = Overflow o (f a)

discardOverflow :: ∀ b a . Overflow b a -> a
discardOverflow (Overflow _ a) = a
