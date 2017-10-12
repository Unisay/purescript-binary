module Data.Bits
  ( class Bits
  , invert
  , add
  , unsafeAdd
  , zero
  , one
  , leftShift
  , unsafeLeftShift
  , rightShift
  , unsafeRightShift
  , toString
  , foldInt
  , toInt
  , sizeOf
  ) where

import Data.Typelevel.Num as N
import Data.BitOverflow (Bit, BitOverflow, discardBitOverflow)
import Data.Typelevel.Num (class LtEq, class Nat, class Pos, d0)
import Data.Typelevel.Num.Aliases (D32)

class Pos n <= Bits n a | a -> n where
  invert :: a -> a
  add :: Bit -> a -> a -> BitOverflow a
  zero :: a
  one :: a
  leftShift :: Bit -> a -> BitOverflow a
  rightShift :: Bit -> a -> BitOverflow a
  toString :: a -> String
  foldInt :: ∀ p. Nat p => p -> a -> Int

toInt :: ∀ a n. Bits n a => LtEq n D32 => a -> Int
toInt = foldInt d0

-- | Unsigned binary addition
-- | Discards overflow bit
unsafeAdd :: ∀ a n. Bits n a => a -> a -> a
unsafeAdd a1 a2 = discardBitOverflow (add false a1 a2)

unsafeRightShift :: ∀ a n. Bits n a => a -> a
unsafeRightShift a = discardBitOverflow (rightShift false a)

unsafeLeftShift :: ∀ a n. Bits n a => a -> a
unsafeLeftShift a = discardBitOverflow (leftShift false a)

sizeOf :: ∀ a n. Bits n a => n -> Int
sizeOf = N.toInt
