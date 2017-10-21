module Data.Binary.Bits
  ( Bits(..)
  , align
  , intToBits
  ) where

import Prelude

import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Binary.Bit (Bit(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..), uncurry)

newtype Bits = Bits (Array Bit)
derive instance newtypeBits :: Newtype Bits _
derive newtype instance eqBits :: Eq Bits
derive newtype instance showBits :: Show Bits
derive newtype instance semigroupBits :: Semigroup Bits

instance ordBits :: Ord Bits where
  compare a b = uncurry compare $ bimap unwrap unwrap $ align a b

-- | Converts a non-negative `Int` value into an `Bits`
intToBits :: Int -> Bits
intToBits = f >>> A.dropWhile (eq (Bit false)) >>> Bits where
  f 0 = [Bit false]
  f n | n `mod` 2 == 1 = A.snoc (f (n `div` 2)) (Bit true)
      | otherwise = A.snoc (f (n `div` 2)) (Bit false)

-- | align length by adding zeroes from the left
align :: Bits -> Bits -> Tuple Bits Bits
align bas@(Bits as) bbs@(Bits bs) =
  case compare la lb of
  EQ -> Tuple bas bbs
  LT -> Tuple (extend (lb - la) as) bbs
  GT -> Tuple bas (extend (la - lb) bs)
  where la = A.length as
        lb = A.length bs
        extend :: Int -> Array Bit -> Bits
        extend d xs = Bits (A.replicate d (Bit false) <> xs)
