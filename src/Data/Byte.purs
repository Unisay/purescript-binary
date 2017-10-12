module Data.Byte
  ( Byte(..)
  , byte0
  , byte1
  , invert
  , unsafeAdd
  , add
  , unsafeAddNibble
  , addNibble
  ) where

import Data.Nibble as N
import Data.Nibble (Bit, Nibble, Overflow, nibble0, nibble1)
import Data.Tuple.Nested (get1, tuple2, (/\))
import Prelude hiding (add)

data Byte = Byte Nibble Nibble

derive instance eqByte :: Eq Byte

instance showByte :: Show Byte where
 show (Byte n1 n2) = show n1 <> show n2

byte0 :: Byte
byte0 = Byte nibble0 nibble0

byte1 :: Byte
byte1 = Byte nibble0 nibble1

invert :: Byte -> Byte
invert (Byte n1 n2) = Byte (N.invert n1) (N.invert n2)

-- | Unsigned binary addition
-- | Looses overflow bit
unsafeAdd :: Byte -> Byte -> Byte
unsafeAdd b1 b2 = get1 (add false b1 b2)

unsafeAddNibble :: Byte -> Nibble -> Byte
unsafeAddNibble byte nibble = get1 (addNibble false byte nibble)

addNibble :: Bit -> Byte -> Nibble -> Overflow Byte
addNibble bit byte nibble = add bit byte (Byte nibble0 nibble)

-- | Unsigned binary addition
-- | Accepts a carry-over bit from the previous addition
-- | Returns resulting byte with overflow bit
add :: Bit -> Byte -> Byte -> Overflow Byte
add bt (Byte a b) (Byte c d) =
  let (f /\ f' /\ _) = N.add bt b d
      (e /\ e' /\ _) = N.add f' a c
  in tuple2 (Byte e f) e'
