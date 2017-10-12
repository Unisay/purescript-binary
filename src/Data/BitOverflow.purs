module Data.BitOverflow where

type Bit = Boolean

data BitOverflow a = BitOverflow Bit a

discardBitOverflow :: ∀ a. BitOverflow a -> a
discardBitOverflow (BitOverflow _ a) = a
