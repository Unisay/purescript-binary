module Data.Binary.Bits
  ( Bits(..)
  , toString
  , zero
  , zeroes
  , one
  , ones
  , head
  , tail
  , init
  , last
  , drop
  , take
  , uncons
  , length
  , setLsb
  , setMsb
  , align
  , addBit
  , addBits
  , addBits'
  , subtractBits
  , extendOverflow
  , intToBits
  , unsafeBitsToInt
  ) where

import Control.Plus (empty)
import Data.Array ((:))
import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Binary.Bit (Bit(..), _0, _1, bitToChar, bitToInt)
import Data.Binary.Overflow (Overflow(..), discardOverflow, makeOverflow, overflowBit)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (abs)
import Data.String as Str
import Data.Tuple (Tuple(..), fst, uncurry)
import Prelude hiding (zero)

newtype Bits = Bits (Array Bit)
derive instance newtypeBits :: Newtype Bits _
derive newtype instance eqBits :: Eq Bits
derive newtype instance showBits :: Show Bits
derive newtype instance semigroupBits :: Semigroup Bits

toString :: Bits -> String
toString (Bits bits) = Str.fromCharArray $ map bitToChar bits

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
        extend d xs = Bits (A.replicate d _0 <> xs)

zero :: Bits
zero = Bits [_0]

zeroes :: Int -> Bits
zeroes n = Bits $ A.replicate (abs n) _0

one :: Bits
one = Bits [_1]

ones :: Int -> Bits
ones n = Bits $ A.replicate (abs n) _1

head :: Bits -> Bit
head (Bits bits) = fromMaybe _0 (A.head bits)

tail :: Bits -> Bits
tail (Bits bits) = defaultBits (A.tail bits)

init :: Bits -> Bits
init (Bits bits) = defaultBits (A.init bits)

last :: Bits -> Bit
last (Bits bits) = fromMaybe _0 (A.last bits)

drop :: Int -> Bits -> Bits
drop n (Bits bits) =
  let xs = A.drop n bits
  in if A.null xs then zero else Bits xs

take :: Int -> Bits -> Bits
take n (Bits bits) =
  let xs = A.take n bits
  in if A.null xs then zero else Bits xs

uncons :: Bits -> { head :: Bit, tail :: Bits }
uncons (Bits bits) = f (A.uncons bits) where
  f (Just { head: h, tail: t }) = { head: h, tail: Bits t }
  f Nothing = { head: _0, tail: zero }

length :: Bits -> Int
length = unwrap >>> A.length

setLsb :: Bit -> Bits -> Bits
setLsb bit (Bits bits) = Bits result where
  result = fromMaybe [bit] updated
  updated = A.updateAt pos bit bits
  pos = len - 1
  len = A.length bits

setMsb :: Bit -> Bits -> Bits
setMsb bit (Bits bits) = Bits result where
  result = fromMaybe [bit] updated
  updated = A.updateAt 0 bit bits

defaultBits :: Maybe (Array Bit) -> Bits
defaultBits (Just []) = zero
defaultBits (Just a) = Bits a
defaultBits Nothing = zero

addBit :: Bit -> Bit -> Bit -> Overflow Bit
addBit (Bit false) (Bit false) (Bit false) = NoOverflow _0
addBit (Bit false) (Bit false) (Bit true)  = NoOverflow _1
addBit (Bit false) (Bit true) (Bit false)  = NoOverflow _1
addBit (Bit false) (Bit true) (Bit true)   = Overflow   _0
addBit (Bit true) (Bit false) (Bit false)  = NoOverflow _1
addBit (Bit true) (Bit false) (Bit true)   = Overflow   _0
addBit (Bit true) (Bit true) (Bit false)   = Overflow   _0
addBit (Bit true) (Bit true) (Bit true)    = Overflow   _1

addBits :: Bit -> Bits -> Bits -> Overflow Bits
addBits bit abits@(Bits as) bbits@(Bits bs) =
  Bits <$> A.foldr f acc pairs where
    f :: Tuple Bit Bit -> Overflow (Array Bit) -> Overflow (Array Bit)
    f (Tuple a b) o = flip A.cons (discardOverflow o) <$> addBit (overflowBit o) a b
    acc = makeOverflow bit []
    pairs = uncurry A.zip $ bimap unwrap unwrap $ align abits bbits

addBits' :: Bit -> Bits -> Bits -> Bits
addBits' bit a b = extendOverflow (addBits bit a b)

subtractBits :: Bits -> Bits -> Bits
subtractBits (Bits as) (Bits bs) = Bits acc where
  f :: (Tuple Bit Bit) -> Tuple Boolean (Array Bit) -> Tuple Boolean (Array Bit)
  -- https://i.stack.imgur.com/5M40R.jpg
  f (Tuple (Bit false) (Bit false)) (Tuple false acc) = Tuple false (A.cons _0 acc)
  f (Tuple (Bit false) (Bit false)) (Tuple true  acc) = Tuple true  (A.cons _1 acc)
  f (Tuple (Bit false) (Bit true) ) (Tuple false acc) = Tuple true  (A.cons _1 acc)
  f (Tuple (Bit false) (Bit true) ) (Tuple true  acc) = Tuple true  (A.cons _0 acc)
  f (Tuple (Bit true)  (Bit false)) (Tuple false acc) = Tuple false (A.cons _1 acc)
  f (Tuple (Bit true)  (Bit false)) (Tuple true  acc) = Tuple false (A.cons _0 acc)
  f (Tuple (Bit true)  (Bit true) ) (Tuple false acc) = Tuple false (A.cons _0 acc)
  f (Tuple (Bit true)  (Bit true) ) (Tuple true  acc) = Tuple true  (A.cons _1 acc)
  (Tuple _ acc) = A.foldr f (Tuple false []) (A.zip as bs)

extendOverflow :: Overflow Bits -> Bits
extendOverflow (NoOverflow bits) = bits
extendOverflow (Overflow (Bits bits)) = Bits (_1 : bits)

intToBits :: Int -> Bits
intToBits 0 = zero
intToBits i = Bits (f i) where
  f 0 = empty
  f n | Int.odd n = A.snoc (f (n `div` 2)) _1
      | otherwise = A.snoc (f (n `div` 2)) _0

unsafeBitsToInt :: Bits -> Int
unsafeBitsToInt (Bits bits) = fst $ A.foldr f (Tuple 0 1) bits where
  f b (Tuple r p) = Tuple (p * bitToInt b + r) (p * 2)
