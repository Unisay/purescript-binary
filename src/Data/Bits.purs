module Data.Bits
  ( class Bits
  , invert
  , add'
  , add
  , unsafeAdd
  , zero
  , one
  , leftShift
  , unsafeLeftShift
  , rightShift
  , unsafeRightShift
  , toString
  , fromString
  , foldInt
  , toInt
  , sizeOf
  ) where

import Data.Typelevel.Num as N
import Data.Bit (Bit, Overflow, discardOverflow)
import Data.Maybe (Maybe)
import Data.Typelevel.Num (class LtEq, class Nat, class Pos, d0)
import Data.Typelevel.Num.Aliases (D32)

class Pos n <= Bits n a | a -> n where
  invert :: a -> a
  add' :: Bit -> a -> a -> Overflow a
  zero :: a
  one :: a
  leftShift :: Bit -> a -> Overflow a
  rightShift :: Bit -> a -> Overflow a
  toString :: a -> String
  fromString :: String -> Maybe a
  foldInt :: ∀ p. Nat p => p -> a -> Int

toInt :: ∀ a n. Bits n a => LtEq n D32 => a -> Int
toInt = foldInt d0

-- | Unsigned binary addition
-- | Returns overflow bit
add :: ∀ a n. Bits n a => a -> a -> Overflow a
add = add' false

-- | Unsigned binary addition
-- | Discards overflow bit
unsafeAdd :: ∀ a n. Bits n a => a -> a -> a
unsafeAdd a1 a2 = discardOverflow (add a1 a2)

unsafeRightShift :: ∀ a n. Bits n a => a -> a
unsafeRightShift a = discardOverflow (rightShift false a)

unsafeLeftShift :: ∀ a n. Bits n a => a -> a
unsafeLeftShift a = discardOverflow (leftShift false a)

sizeOf :: ∀ a n. Bits n a => n -> Int
sizeOf = N.toInt
