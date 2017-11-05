module Data.Binary
  ( module Bit
  , module Bits
  , module Overflow
  , class Binary
  , lsb
  , msb
  , _0
  , _1
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
  , toBinString
  , tryFromBinString
  , toBits
  , tryFromBits
  , class Elastic
  , fromBits
  , unsafeFromBits
  , addLeadingBit
  , addLeadingZeros
  , stripLeadingZeros
  , tryFromBinStringElastic
  , Radix(Radix)
  , bin
  , oct
  , dec
  , hex
  ) where

import Data.Array as A
import Data.Binary.Bit as Bit
import Data.Binary.Bit (Bit(..), bitToChar, charToBit)
import Data.Binary.Bits as Bits
import Data.Binary.Bits (Bits(..))
import Data.Binary.Overflow as Overflow
import Data.HeytingAlgebra as HA
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, fromMaybe')
import Data.Newtype (unwrap, wrap)
import Data.String as Str
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), snd)
import Partial.Unsafe (unsafeCrashWith)
import Prelude hiding (not)


class  Binary a where
  _0 :: a
  _1 :: a
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

-- instance binaryInt :: Binary Int where
--   _0 = 0
--   _1 = 1
--   lsb i = intToBit $ Int.and i 1
--   msb i = intToBit $ Int.zshr i 31
--   and = Int.and
--   xor = Int.xor
--   or  = Int.or
--   not = Int.xor top
--   leftShift (Bit b) i = Tuple (msb i) res where
--     res = ifelse b 1 0 `or` Int.shl i 1
--   rightShift (Bit b) i = Tuple (lsb i) res where
--     res = ifelse b bottom 0 `or` Int.shr i 1
--   signedRightShift i = rightShift (intToBit $ Int.and bottom i) i
--   toBits = intBits >>> addLeadingBit 31 _0 >>> Bits where
--     intBits 0 = [_0]
--     intBits n | odd n     = A.snoc (intBits (n `div` 2)) _1
--               | otherwise = A.snoc (intBits (n `div` 2)) _0
--   tryFromBits (Bits bs) = Just $ get1 $ A.foldr f (Tuple 0 1) bs where
--     f b (Tuple r p) = Tuple (p * bitToInt b + r) (p * 2)

instance binaryBit :: Binary Bit where
  _0 = Bit false
  _1 = Bit true
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
  _0 = Bits (pure _0)
  _1 = Bits (pure _1)
  lsb (Bits as) = fromMaybe _0 (A.last as)
  msb (Bits as) = fromMaybe _0 (A.head as)
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

toBinString :: ∀ a. Binary a => a -> String
toBinString = toBits
          >>> unwrap
          >>> map bitToChar
          >>> Str.fromCharArray

tryFromBinString :: ∀ a. Binary a => String -> Maybe a
tryFromBinString = Str.toCharArray
               >>> traverse charToBit
               >=> Bits
               >>> tryFromBits

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
  fromBits (Bits []) = _0
  fromBits bs = bs

addLeadingZeros :: ∀ a. Elastic a => Int -> a -> a
addLeadingZeros w = toBits
                >>> unwrap
                >>> addLeadingBit w _0
                >>> wrap
                >>> fromBits

addLeadingBit :: Int -> Bit -> Array Bit -> Array Bit
addLeadingBit width bit bits =
  let d = sub width (A.length bits)
  in if d < 1 then bits else (A.replicate d bit) <> bits

stripLeadingZeros :: ∀ a. Elastic a => a -> a
stripLeadingZeros = toBits
                >>> unwrap
                >>> stripLeadingBit _0
                >>> wrap
                >>> fromBits

stripLeadingBit :: Bit -> Array Bit -> Array Bit
stripLeadingBit bit bits =
  let xs = A.dropWhile (eq bit) bits
  in if A.null xs then [_0] else xs

tryFromBinStringElastic :: ∀ a. Elastic a => String -> Maybe a
tryFromBinStringElastic =
  Str.toCharArray >>> traverse charToBit >>> map wrap >>> map fromBits

-- | The number of unique digits (including zero) used to represent integers in
-- | a specific base.

newtype Radix = Radix Bits

derive newtype instance eqRadix :: Eq Radix
derive newtype instance showRadix :: Show Radix

-- | The base-2 system.
bin :: Radix
bin = Radix (Bits [_1, _0])

-- | The base-8 system.
oct :: Radix
oct = Radix (Bits [_1, _0, _0, _0])

-- | The base-10 system.
dec :: Radix
dec = Radix (Bits [_1, _0, _1, _0])

-- | The base-16 system.
hex :: Radix
hex = Radix (Bits [_1, _0, _0, _0, _0])
