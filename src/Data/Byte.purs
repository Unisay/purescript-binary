module Data.Byte
  ( Byte(..)
  , unsafeAddNibble
  , addNibble
  ) where

import Control.Apply ((<$>), (<*>))
import Data.Bit (Bit, Overflow(..), discardOverflow)
import Data.Bits (class Bits, add', foldInt, fromString, invert, leftShift, one, rightShift, toInt, toString, zero)
import Data.Eq (class Eq, (/=))
import Data.Maybe (Maybe(..))
import Data.Nibble (Nibble)
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Show (class Show, show)
import Data.String (drop, length, take)
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
    let (Overflow n2' _n2) = leftShift b n2
        (Overflow n1' _n1) = leftShift n2' n1
    in Overflow n1' (Byte _n1 _n2)

  rightShift b (Byte n1 n2) =
    let (Overflow n2' _n2) = rightShift b n2
        (Overflow n1' _n1) = rightShift n2' n1
    in Overflow n1' (Byte _n1 _n2)

  toString (Byte n1 n2) = show n1 <> show n2

  fromString s =
    if length s /= 8
    then Nothing
    else Byte <$> fromString (take 4 s)
              <*> fromString (drop 4 s)

  foldInt p (Byte n1 n2) = toInt n2 + foldInt d4 n1

  -- | Unsigned binary addition
  -- | Accepts a carry-over bit from the previous addition
  -- | Returns resulting byte with overflow bit
  add' bt (Byte a b) (Byte c d) =
    let (Overflow f' f) = add' bt b d
        (Overflow e' e) = add' f' a c
    in Overflow e' (Byte e f)



-- | Discards overflow bit
unsafeAddNibble :: Byte -> Nibble -> Byte
unsafeAddNibble byte nibble = discardOverflow (addNibble false byte nibble)

addNibble :: Bit -> Byte -> Nibble -> Overflow Byte
addNibble bit byte nibble = add' bit byte (Byte zero nibble)
