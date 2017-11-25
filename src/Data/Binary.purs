module Data.Binary
  ( module Bit
  , module Bits
  , module Overflow
  , class Binary
  , lsb
  , msb
  , isOdd
  , isEven
  , isZero
  , and
  , xor
  , or
  , not
  , leftShift
  , unsafeLeftShift
  , rightShift
  , unsafeRightShift
  , toBits
  , tryFromBits
  , class Elastic
  , fromBits
  , unsafeFromBits
  , addLeadingBit
  , addLeadingZeros
  , stripLeadingBit
  , stripLeadingZeros
  , tryFromBinStringElastic
  ) where

import Prelude hiding (not, zero)

import Data.Array as A
import Data.Binary.Bit (Bit(Bit), _0, _1, charToBit)
import Data.Binary.Bit as Bit
import Data.Binary.Bits (Bits(..), zero)
import Data.Binary.Bits as Bits
import Data.Binary.Overflow as Overflow
import Data.HeytingAlgebra as HA
import Data.Maybe (Maybe(Just, Nothing), fromMaybe')
import Data.Newtype (unwrap, wrap)
import Data.String as Str
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), snd)
import Partial.Unsafe (unsafeCrashWith)


class Binary a where
  -- | Least significant bit
  lsb :: a -> Bit
  -- | most significant bit
  msb :: a -> Bit
  and :: a -> a -> a
  xor :: a -> a -> a
  or  :: a -> a -> a
  not :: a -> a
  -- add' :: Bit -> a -> a -> Overflow a
  -- subtract :: a -> a -> a
  leftShift :: Bit -> a -> Tuple Bit a
  rightShift :: Bit -> a -> Tuple Bit a
  toBits :: a -> Bits
  tryFromBits :: Bits -> Maybe a

instance binaryBit :: Binary Bit where
  lsb = id
  msb = id
  and (Bit a) (Bit b) = Bit (a && b)
  xor (Bit a) (Bit b) = Bit ((a || b) && HA.not (a && b))
  or  (Bit a) (Bit b) = Bit (a || b)
  not (Bit b) = Bit (HA.not b)
  leftShift b a = Tuple a b
  rightShift b a = Tuple a b
  toBits = A.singleton >>> Bits
  tryFromBits (Bits [b]) = Just b
  tryFromBits _ = Nothing

instance binaryBits :: Binary Bits where
  lsb = Bits.last
  msb = Bits.head
  and (Bits as) (Bits bs) = Bits (A.zipWith and as bs)
  xor (Bits as) (Bits bs) = Bits (A.zipWith xor as bs)
  or  (Bits as) (Bits bs) = Bits (A.zipWith or as bs)
  not (Bits bits) = Bits (map not bits)
  leftShift bit (Bits bits) = Bits <$> A.foldr f (Tuple bit []) bits
    where f a (Tuple o t) = flip A.cons t <$> leftShift o a
  rightShift bit (Bits bits) = Bits <$> A.foldl f (Tuple bit []) bits
    where f (Tuple o t) a = A.snoc t <$> rightShift o a
  toBits = id
  tryFromBits = Just <<< fromBits

isOdd :: ∀ a. Binary a => a -> Boolean
isOdd = lsb >>> eq _1

isEven :: ∀ a. Binary a => a -> Boolean
isEven = lsb >>> eq _0

isZero :: ∀ a. Binary a => a -> Boolean
isZero = toBits >>> unwrap >>> A.dropWhile (eq _0) >>> A.null

unsafeFromBits :: ∀ a. Binary a => Bits -> a
unsafeFromBits bits = fromMaybe' (\_ -> unsafeCrashWith err) (tryFromBits bits) where
  err = "Unsafe conversion of Bits to a binary value has failed: " <> show bits

unsafeRightShift :: ∀ a. Binary a => a -> a
unsafeRightShift a = snd (rightShift _0 a)

unsafeLeftShift :: ∀ a. Binary a => a -> a
unsafeLeftShift a = snd (leftShift _0 a)


class Binary a <= Elastic a where
  fromBits :: Bits -> a

instance elasticBits :: Elastic Bits where
  fromBits (Bits []) = zero
  fromBits bs = bs

addLeadingZeros :: ∀ a. Elastic a => Int -> a -> a
addLeadingZeros = addLeadingBit _0

addLeadingBit :: ∀ a. Elastic a => Bit -> Int -> a -> a
addLeadingBit b w =
  toBits >>> unwrap >>> addLeadingBitArray b w >>> wrap >>> fromBits
  where addLeadingBitArray bit width bits =
          let d = sub width (A.length bits)
          in if d < 1 then bits else (A.replicate d bit) <> bits

stripLeadingZeros :: ∀ a. Elastic a => a -> a
stripLeadingZeros = stripLeadingBit _0

stripLeadingBit :: ∀ a. Elastic a => Bit -> a -> a
stripLeadingBit b =
  toBits >>> unwrap >>> stripLeadingBitArray b >>> wrap >>> fromBits
  where stripLeadingBitArray bit bits =
          let xs = A.dropWhile (eq bit) bits
          in if A.null xs then [_0] else xs

tryFromBinStringElastic :: ∀ a. Elastic a => String -> Maybe a
tryFromBinStringElastic =
  Str.toCharArray >>> traverse charToBit >>> map wrap >>> map fromBits
