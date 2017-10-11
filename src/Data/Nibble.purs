module Data.Nibble
  ( Nibble (..)
  , Bit
  , n0
  , toString
  , toInt
  , invert
  , lshift
  , lshift'
  , rshift
  , rshift'
  ) where

import Prelude

import Data.Tuple.Nested (Tuple2, tuple2)

type Bit = Boolean
data Nibble = Nibble Bit Bit Bit Bit

derive instance eqNibble :: Eq Nibble

instance showNibble :: Show Nibble where
  show = toString

instance ordNibble :: Ord Nibble where
  compare l r = compare (toString l) (toString r)

instance boundedNibble :: Bounded Nibble where
  bottom = n0
  top = Nibble true true true true

n0 :: Nibble
n0 = Nibble false false false false

invert :: Nibble -> Nibble
invert (Nibble a b c d) = Nibble (not a) (not b) (not c) (not d)

lshift :: Bit -> Nibble -> Nibble
lshift z (Nibble _ b c d) = Nibble b c d z

lshift' :: Bit -> Nibble -> Tuple2 Nibble Bit
lshift' z (Nibble a b c d) = tuple2 (Nibble b c d z) a

rshift :: Bit -> Nibble -> Nibble
rshift z (Nibble a b c _) = Nibble z a b c

rshift' :: Bit -> Nibble -> Tuple2 Nibble Bit
rshift' z (Nibble a b c d) = tuple2 (Nibble z a b c) d

toString :: Nibble -> String
toString (Nibble a b c d) = bit a <> bit b <> bit c <> bit d
  where bit = if _ then "1" else "0"

toInt :: Nibble -> Int
toInt (Nibble a b c d) = bit d + 2 * (bit c) + 4 * (bit b) + 8 * (bit a)
  where bit = if _ then 1 else 0
