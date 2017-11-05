module Data.Binary.Overflow
  ( Overflow(..)
  , overflowBit
  , makeOverflow
  , discardOverflow
  ) where


import Prelude

import Data.Binary.Bit (Bit(..))

data Overflow a = Overflow a | NoOverflow a

derive instance eqOverflow :: Eq a => Eq (Overflow a)

instance showOverflow :: Show a => Show (Overflow a) where
  show (Overflow a) = show a <> " with overflow"
  show (NoOverflow a) = show a <> " without overflow"

instance functorOverflow :: Functor Overflow where
  map f (Overflow a) = Overflow (f a)
  map f (NoOverflow a) = NoOverflow (f a)

overflowBit :: ∀ a . Overflow a -> Bit
overflowBit (Overflow _)   = Bit true
overflowBit (NoOverflow _) = Bit false

makeOverflow :: ∀ a . Bit -> a -> Overflow a
makeOverflow (Bit true) a = Overflow a
makeOverflow (Bit false) a = NoOverflow a

discardOverflow :: ∀ a . Overflow a -> a
discardOverflow (Overflow a) = a
discardOverflow (NoOverflow a) = a
