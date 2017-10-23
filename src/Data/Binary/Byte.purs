module Data.Binary.Byte
  ( Byte(..)
  ) where

import Data.Binary.Class (class Binary, class FitsInt, class Fixed, Bit(Bit), Bits(Bits), _0, _1, add, add', addLeadingZeros, diffFixed, invert, leftShift, rightShift, toBinString, toBits, toInt, unsafeAdd)
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

  invert (Byte n1 n2) = Byte (invert n1) (invert n2)

  toBits (Byte h l) = toBits h <> toBits l

  leftShift b (Byte h l) =
    let (Overflow o l') = leftShift b l
    in flip Byte l' <$> leftShift o h

  rightShift b (Byte h l) =
    let (Overflow o h') = rightShift b h
    in Byte h' <$> rightShift o l

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
  modAdd a b = f (add a b) where
    f (Overflow (Bit false) x) = x
    f (Overflow (Bit true) x) = unsafeAdd x _1


instance fitsIntByte :: FitsInt Byte where
  toInt (Byte h l) = 16 * toInt h + toInt l
