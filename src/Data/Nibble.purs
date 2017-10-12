module Data.Nibble
  ( Nibble (..)
  ) where

import Data.BitOverflow (Bit, BitOverflow(BitOverflow))
import Data.Bits (class Bits, toString)
import Data.Typelevel.Num as N
import Data.Typelevel.Num (D4)
import Data.Int (pow)
import Prelude hiding (add)

data Nibble = Nibble Bit Bit Bit Bit

derive instance eqNibble :: Eq Nibble

instance showNibble :: Show Nibble where
  show n = toString n

instance ordNibble :: Ord Nibble where
  compare l r = compare (toString l) (toString r)

instance bitsNibble :: Bits D4 Nibble where
  invert (Nibble a b c d) = Nibble (not a) (not b) (not c) (not d)

  -- | Unsigned binary addition
  -- | Accepts a carry-over bit from the previous addition
  -- | Returns resulting `Nibble` with overflow bit
  add b (Nibble a0 b0 c0 d0) (Nibble a1 b1 c1 d1) =
    let (BitOverflow d' d) = addBits d0 d1 b
        (BitOverflow c' c) = addBits c0 c1 d'
        (BitOverflow b' b) = addBits b0 b1 c'
        (BitOverflow a' a) = addBits a0 a1 b'
    in BitOverflow a' (Nibble a b c d) where

    addBits :: Bit -> Bit -> Bit -> BitOverflow Bit
    addBits false false false = BitOverflow false false
    addBits false false true  = BitOverflow false true
    addBits false true  false = BitOverflow false true
    addBits false true  true  = BitOverflow true false
    addBits true  false false = BitOverflow false true
    addBits true  false true  = BitOverflow true false
    addBits true  true  false = BitOverflow true false
    addBits true  true  true  = BitOverflow true true

  zero = Nibble false false false false
  one = Nibble false false false true
  leftShift z (Nibble a b c d) = BitOverflow a (Nibble b c d z)
  rightShift z (Nibble a b c d) = BitOverflow d (Nibble z a b c)
  toString (Nibble a b c d) = bit a <> bit b <> bit c <> bit d
    where bit = if _ then "1" else "0"
  foldInt p (Nibble a b c d) =
    let i = N.toInt p
        bit = if _ then 1 else 0
        base = pow 2
    in (base (i + 0)) * (bit d)
     + (base (i + 1)) * (bit c)
     + (base (i + 2)) * (bit b)
     + (base (i + 3)) * (bit a)
