module Data.Binary.Nibble
  ( Nibble(..)
  ) where

import Conditional (ifelse)
import Data.Binary.Class (class Binary, class FitsInt, class Fixed, Bit(Bit), Bits(Bits), _0, _1, add', addLeadingZeros, and, invert, or, toBinString, toBits, xor)
import Data.Binary.Overflow (Overflow(..))
import Data.Maybe (Maybe(..))
import Prelude hiding (add)


data Nibble = Nibble Bit Bit Bit Bit

derive instance eqNibble :: Eq Nibble
instance ordNibble :: Ord Nibble where
  compare a b = compare (toBits a) (toBits b)

instance showNibble :: Show Nibble where
  show = toBinString

instance boundedNibble :: Bounded Nibble where
  top = Nibble top top top top
  bottom = Nibble bottom bottom bottom bottom

instance binaryNibble :: Binary Nibble where
  _0 = Nibble _0 _0 _0 _0
  _1 = Nibble _0 _0 _0 _1
  and (Nibble a0 b0 c0 d0) (Nibble a1 b1 c1 d1) =
    Nibble (and a0 a1) (and b0 b1) (and c0 c1) (and d0 d1)
  xor (Nibble a0 b0 c0 d0) (Nibble a1 b1 c1 d1) =
    Nibble (xor a0 a1) (xor b0 b1) (xor c0 c1) (xor d0 d1)
  or (Nibble a0 b0 c0 d0) (Nibble a1 b1 c1 d1) =
    Nibble (or a0 a1) (or b0 b1) (or c0 c1) (or d0 d1)
  invert (Nibble a b c d) =
    Nibble (invert a) (invert b) (invert c) (invert d)
  -- | Unsigned binary addition
  -- | Accepts a carry-over bit from the previous addition
  -- | Returns resulting `Nibble` with overflow bit
  add' b (Nibble a0 b0 c0 d0) (Nibble a1 b1 c1 d1) =
    let (Overflow d' d) = add' d0 d1 b
        (Overflow c' c) = add' c0 c1 d'
        (Overflow b' b) = add' b0 b1 c'
        (Overflow a' a) = add' a0 a1 b'
    in Overflow a' (Nibble a b c d)
  leftShift z (Nibble a b c d) = Overflow a (Nibble b c d z)
  rightShift z (Nibble a b c d) = Overflow d (Nibble z a b c)
  toBits (Nibble a b c d) = Bits [a, b, c, d]

instance fixedNibble :: Fixed Nibble where
  numBits _ = 4
  tryFromBits = addLeadingZeros 4 >>> f where
    f (Bits [a, b, c, d]) = Just (Nibble a b c d)
    f _ = Nothing

instance fitsIntNibble :: FitsInt Nibble where
  toInt (Nibble a b c d) = 8 * bin a + 4 * bin b + 2 * bin c + bin d where
    bin (Bit bt) = ifelse bt 1 0
