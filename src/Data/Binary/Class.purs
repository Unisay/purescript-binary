module Data.Binary.Class
  ( module Bit
  , class Binary
  , class Fixed
  , class FitsInt
  , _0
  , _1
  , isOdd
  , isEven
  , invert
  , add'
  , add
  , unsafeAdd
  , leftShift
  , unsafeLeftShift
  , rightShift
  , unsafeRightShift
  , toBinString
  , tryFromBinString
  , toInt
  , tryToInt
  , tryFromInt
  , toBits
  , tryFromBits
  , numBits
  ) where

import Conditional (ifelse)
import Control.Plus (empty)
import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Binary.Bit (Bit(..), bitToChar, charToBit)
import Data.Binary.Bit as Bit
import Data.Binary.Bits (Bits(..), align, intToBits)
import Data.Binary.Overflow (Overflow(..), discardOverflow)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (unwrap)
import Data.String (fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), uncurry)
import Data.Tuple.Nested (get1)
import Prelude hiding (add)
import Type.Proxy (Proxy)


class Ord a <= Binary a where
  _0 :: a
  _1 :: a
  invert :: a -> a
  add' :: Bit -> a -> a -> Overflow Bit a
  leftShift :: Bit -> a -> Overflow Bit a
  rightShift :: Bit -> a -> Overflow Bit a
  toBits :: a -> Bits

instance binaryBits :: Binary Bits where
  _0 = Bits (pure _0)
  _1 = Bits (pure _1)

  invert (Bits bits) = Bits (map invert bits)

  add' bit abits@(Bits as) bbits@(Bits bs) =
    Bits <$> A.foldr f acc pairs where
      f (Tuple a b) (Overflow o t) = flip A.cons t <$> add' o a b
      acc = Overflow bit empty
      pairs = uncurry A.zip $ bimap unwrap unwrap $ align abits bbits

  leftShift bit (Bits bits) = Bits <$> A.foldr f (Overflow bit []) bits
    where f a (Overflow o t) = flip A.cons t <$> leftShift o a

  rightShift bit (Bits bits) = Bits <$> A.foldl f (Overflow bit []) bits
    where f (Overflow o t) a = A.snoc t <$> rightShift o a

  toBits = id


isOdd :: ∀ a. Binary a => a -> Boolean
isOdd = toBits >>> unwrap >>> A.last >>> maybe false (eq _1)

isEven :: ∀ a. Binary a => a -> Boolean
isEven = toBits >>> unwrap >>> A.last >>> maybe true (eq _0)

toBinString :: ∀ a. Binary a => a -> String
toBinString = toBits >>> unwrap >>> map bitToChar >>> fromCharArray

tryFromBinString :: ∀ a. Fixed a => String -> Maybe a
tryFromBinString =
  toCharArray >>> traverse charToBit >=> Bits >>> tryFromBits

class Binary a <= FitsInt a where
  toInt :: a -> Int

class Binary a <= Fixed a where
  numBits :: Proxy a -> Int
  tryFromBits :: Bits -> Maybe a

tryToInt :: ∀ a. Binary a => a -> Maybe Int
tryToInt binary | (Bits bts) <- toBits binary =
  bitsToInt (A.length bts) bts where
    bitsToInt l _ | l > 31 = Nothing
    bitsToInt 0 _ = Just 0
    bitsToInt _ bits = Just $ get1 $ A.foldr f (Tuple 0 1) bits
    f b (Tuple r p) = Tuple (p * toInt b + r) (p * 2)

tryFromInt :: ∀ a. Fixed a => Int -> Maybe a
tryFromInt = intToBits >>> tryFromBits


instance binaryBit :: Binary Bit where
  _0 = Bit false
  _1 = Bit true

  invert (Bit b) = Bit (not b)

  add' (Bit false) (Bit false) (Bit false) = Overflow _0 _0
  add' (Bit false) (Bit false) (Bit true)  = Overflow _0 _1
  add' (Bit false) (Bit true) (Bit false)  = Overflow _0 _1
  add' (Bit false) (Bit true) (Bit true)   = Overflow _1 _0
  add' (Bit true) (Bit false) (Bit false)  = Overflow _0 _1
  add' (Bit true) (Bit false) (Bit true)   = Overflow _1 _0
  add' (Bit true) (Bit true) (Bit false)   = Overflow _1 _0
  add' (Bit true) (Bit true) (Bit true)    = Overflow _1 _1

  toBits = A.singleton >>> Bits

  leftShift b a = Overflow a b
  rightShift b a = Overflow a b

instance fixedBit :: Fixed Bit where
  numBits _ = 1

  tryFromBits (Bits [b]) = Just b
  tryFromBits _ = Nothing

instance fitsIntBit :: FitsInt Bit where
  toInt (Bit b) = ifelse b 1 0

-- | Unsigned binary addition
-- | Returns overflow bit
add :: ∀ a. Binary a => a -> a -> Overflow Bit a
add = add' _0

-- | Unsigned binary addition
-- | Discards overflow bit
unsafeAdd :: ∀ a. Binary a => a -> a -> a
unsafeAdd a1 a2 = discardOverflow (add a1 a2)

unsafeRightShift :: ∀ a. Binary a => a -> a
unsafeRightShift a = discardOverflow (rightShift _0 a)

unsafeLeftShift :: ∀ a. Binary a => a -> a
unsafeLeftShift a = discardOverflow (leftShift _0 a)
