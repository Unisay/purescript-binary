module Data.Byte
  ( Byte(..)
  , unsafeAddNibble
  , addNibble
  ) where

import Data.BitOverflow (Bit, BitOverflow(..), discardBitOverflow)
import Data.Bits (class Bits, add, foldInt, invert, leftShift, one, rightShift, toInt, toString, zero)
import Data.Eq (class Eq)
import Data.Nibble (Nibble)
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Show (class Show, show)
import Data.Typelevel.Num (D8, d4)

data Byte = Byte Nibble Nibble

derive instance eqByte :: Eq Byte

instance showByte :: Show Byte where
 show = toString

instance bitsByte :: Bits D8 Byte where
  invert (Byte n1 n2) = Byte (invert n1) (invert n2)
  zero = Byte zero zero
  one = Byte zero one
  leftShift b (Byte n1 n2) =
    let (BitOverflow n2' _n2) = leftShift b n2
        (BitOverflow n1' _n1) = leftShift n2' n1
    in BitOverflow n1' (Byte _n1 _n2)
  rightShift b (Byte n1 n2) =
    let (BitOverflow n2' _n2) = rightShift b n2
        (BitOverflow n1' _n1) = rightShift n2' n1
    in BitOverflow n1' (Byte _n1 _n2)
  toString (Byte n1 n2) = show n1 <> show n2
  foldInt p (Byte n1 n2) = toInt n2 + foldInt d4 n1

  -- | Unsigned binary addition
  -- | Accepts a carry-over bit from the previous addition
  -- | Returns resulting byte with overflow bit
  add bt (Byte a b) (Byte c d) =
    let (BitOverflow f' f) = add bt b d
        (BitOverflow e' e) = add f' a c
    in BitOverflow e' (Byte e f)



-- | Discards overflow bit
unsafeAddNibble :: Byte -> Nibble -> Byte
unsafeAddNibble byte nibble = discardBitOverflow (addNibble false byte nibble)

addNibble :: Bit -> Byte -> Nibble -> BitOverflow Byte
addNibble bit byte nibble = add bit byte (Byte zero nibble)
