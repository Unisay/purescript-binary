module Data.Binary.Overflow
  ( Overflow(..)
  , overflow
  , discardOverflow
  ) where

import Prelude

data Overflow b a = Overflow b a

derive instance eqOverflow :: (Eq b, Eq a) => Eq (Overflow b a)

instance showOverflow :: (Show b, Show a) => Show (Overflow b a) where
  show (Overflow b a) = show a <> " with " <> show b <> " overflow"

instance functorOverflow :: Functor (Overflow b) where
  map f (Overflow o a) = Overflow o (f a)

overflow :: ∀ b a . Overflow b a -> b
overflow (Overflow b _) = b

discardOverflow :: ∀ b a . Overflow b a -> a
discardOverflow (Overflow _ a) = a
