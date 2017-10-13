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

  leftShift b (Byte h l) =
    let (Overflow o l') = leftShift b l
        (Overflow o' h') = leftShift o h
    in Overflow o' (Byte h' l')

  rightShift b (Byte h l) =
    let (Overflow o h') = rightShift b h
        (Overflow o' l') = rightShift o l
    in Overflow o' (Byte h' l')

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
  add' o (Byte h l) (Byte h' l') =
    let (Overflow o _l) = add' o l l'
        (Overflow _o _h) = add' o h h'
    in Overflow _o (Byte _h _l)


-- | Discards overflow bit
unsafeAddNibble :: Byte -> Nibble -> Byte
unsafeAddNibble byte nibble = discardOverflow (addNibble false byte nibble)

addNibble :: Bit -> Byte -> Nibble -> Overflow Byte
addNibble bit byte nibble = add' bit byte (Byte zero nibble)
