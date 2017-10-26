module Data.Binary.Byte
  ( Byte(..)
  ) where


import Data.Binary.Class (class Binary, class FitsInt, class Fixed, Bits(Bits), _0, _1, add', addLeadingZeros, and, invert, leftShift, or, rightShift, toBinString, toBits, toInt, xor)
import Data.Binary.Nibble (Nibble(..))
import Data.Binary.Overflow (Overflow(..))
import Data.Maybe (Maybe(..))
import Prelude hiding (add)

data Byte = Byte Nibble Nibble

derive instance eqByte :: Eq Byte

instance showByte :: Show Byte where
 show = toBinString

instance ordByte :: Ord Byte where
  compare l r = compare (toBits l) (toBits r)

instance boundedByte :: Bounded Byte where
  top = Byte top top
  bottom = Byte bottom bottom

instance binaryByte :: Binary Byte where
  _0 = Byte _0 _0
  _1 = Byte _0 _1
  and (Byte a b) (Byte c d) = Byte (and a c) (and b d)
  xor (Byte a b) (Byte c d) = Byte (xor a c) (xor b d)
  or (Byte a b) (Byte c d) = Byte (or a c) (or b d)
  invert (Byte n1 n2) = Byte (invert n1) (invert n2)
  leftShift b (Byte h l) =
    let (Overflow o l') = leftShift b l
    in flip Byte l' <$> leftShift o h
  rightShift b (Byte h l) =
    let (Overflow o h') = rightShift b h
    in Byte h' <$> rightShift o l
  toBits (Byte h l) = toBits h <> toBits l

  -- | Unsigned binary addition
  -- | Accepts a carry-over bit from the previous addition
  -- | Returns resulting byte with overflow bit
  add' o (Byte h l) (Byte h' l') =
    let (Overflow o' l'') = add' o l l'
    in flip Byte l'' <$> add' o' h h'

instance fixedByte :: Fixed Byte where
  numBits _ = 8
  tryFromBits = addLeadingZeros 8 >>> p where
    p (Bits [a, b, c, d, e, f, g, h]) =
      Just (Byte (Nibble a b c d) (Nibble e f g h))
    p _ = Nothing

instance fitsIntByte :: FitsInt Byte where
  toInt (Byte h l) = 16 * toInt h + toInt l
