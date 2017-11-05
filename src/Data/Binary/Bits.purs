module Data.Binary.Bits
  ( Bits(..)
  , head
  , tail
  , init
  , last
  , drop
  , take
  , uncons
  , length
  , align
  ) where

import Prelude

import Data.Array as A
import Data.Binary.Bit (Bit(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))

newtype Bits = Bits (Array Bit)
derive instance newtypeBits :: Newtype Bits _
derive newtype instance eqBits :: Eq Bits
derive newtype instance showBits :: Show Bits
derive newtype instance semigroupBits :: Semigroup Bits

-- instance ordBits :: Ord Bits where
--   compare a b = f (isNegative a) (isNegative b) where
--     f true true = cmp
--     f false false = cmp
--     f true false = LT
--     f false true = GT
--     cmp = uncurry compare $ bimap unwrap unwrap $ align a b

-- instance semiringBits :: Semiring Bits where
--   zero = _0
--   add = extendAdd
--   one = _1
--   mul = multiply

-- instance ringBits :: Ring Bits where
--   sub = subtractElastic

-- complement :: Bits -> Bits
-- complement = invert >>> unsafeAdd _1

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

empty :: Bits
empty = Bits [(Bit false)]

head :: Bits -> Bit
head (Bits bits) = fromMaybe (Bit false) (A.head bits)

tail :: Bits -> Bits
tail (Bits bits) = defaultBits (A.tail bits)

init :: Bits -> Bits
init (Bits bits) = defaultBits (A.init bits)

last :: Bits -> Bit
last (Bits bits) = fromMaybe (Bit false) (A.last bits)

drop :: Int -> Bits -> Bits
drop n (Bits bits) =
  let xs = A.drop n bits
  in if A.null xs then empty else Bits xs

take :: Int -> Bits -> Bits
take n (Bits bits) =
  let xs = A.take n bits
  in if A.null xs then empty else Bits xs

uncons :: Bits -> { head :: Bit, tail :: Bits }
uncons (Bits bits) = f (A.uncons bits) where
  f (Just { head: h, tail: t }) = { head: h, tail: Bits t }
  f Nothing = { head: Bit false, tail: empty }

length :: Bits -> Int
length = unwrap >>> A.length

defaultBits :: Maybe (Array Bit) -> Bits
defaultBits (Just []) = empty
defaultBits (Just a) = Bits a
defaultBits Nothing = empty
