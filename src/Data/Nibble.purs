module Data.Nibble
  ( Nibble (..)
  , Bit
  , Overflow
  , nibble0
  , nibble1
  , toString
  , toInt
  , invert
  , add
  , unsafeAdd
  , lshift
  , lshift'
  , rshift
  , rshift'
  ) where

import Prelude hiding (add)
import Data.Tuple.Nested (Tuple2, get1, tuple2, (/\))

type Bit = Boolean
type Overflow a = Tuple2 a Bit

data Nibble = Nibble Bit Bit Bit Bit

derive instance eqNibble :: Eq Nibble

instance showNibble :: Show Nibble where
  show = toString

instance ordNibble :: Ord Nibble where
  compare l r = compare (toString l) (toString r)

-- -- | **Note:** The `Nibble` is not a fully law abiding
-- -- | member of this class hierarchy due to the potential for arithmetic
-- -- | overflows. The behaviour is unspecified in these case.
-- instance semiringNibble :: Semiring Nibble where
--   zero = Nibble false false false false
--   one = Nibble false false false true
--   add a b = get1 (addOver false a b) -- throws away overflow bit
--   mul (Nibble a b c d) (Nibble a' b' c' d') = unsafeCoerce unit

nibble0 :: Nibble
nibble0 = Nibble false false false false

nibble1 :: Nibble
nibble1 = Nibble false false false true

-- | Unsigned binary addition
-- | Looses overflow bit
unsafeAdd :: Nibble -> Nibble -> Nibble
unsafeAdd n1 n2 = get1 (add false n1 n2)

-- | Unsigned binary addition
-- | Accepts a carry-over bit from the previous addition
-- | Returns resulting `Nibble` with overflow bit
add :: Bit -> Nibble -> Nibble -> Overflow Nibble
add b (Nibble a0 b0 c0 d0) (Nibble a1 b1 c1 d1) =
  let (_d /\ d' /\ _) = addBits d0 d1 b
      (_c /\ c' /\ _) = addBits c0 c1 d'
      (_b /\ b' /\ _) = addBits b0 b1 c'
      (_a /\ a' /\ _) = addBits a0 a1 b'
  in tuple2 (Nibble _a _b _c _d) a' where

  addBits :: Bit -> Bit -> Bit -> Overflow Bit
  addBits false false false = tuple2 false false
  addBits false false true  = tuple2 true  false
  addBits false true  false = tuple2 true  false
  addBits false true  true  = tuple2 false true
  addBits true  false false = tuple2 true  false
  addBits true  false true  = tuple2 false true
  addBits true  true  false = tuple2 false true
  addBits true  true  true  = tuple2 true  true


invert :: Nibble -> Nibble
invert (Nibble a b c d) = Nibble (not a) (not b) (not c) (not d)

lshift :: Bit -> Nibble -> Nibble
lshift z (Nibble _ b c d) = Nibble b c d z

lshift' :: Bit -> Nibble -> Overflow Nibble
lshift' z (Nibble a b c d) = tuple2 (Nibble b c d z) a

rshift :: Bit -> Nibble -> Nibble
rshift z (Nibble a b c _) = Nibble z a b c

rshift' :: Bit -> Nibble -> Overflow Nibble
rshift' z (Nibble a b c d) = tuple2 (Nibble z a b c) d

toString :: Nibble -> String
toString (Nibble a b c d) = bit a <> bit b <> bit c <> bit d
  where bit = if _ then "1" else "0"

toInt :: Nibble -> Int
toInt (Nibble a b c d) = bit d + 2 * (bit c) + 4 * (bit b) + 8 * (bit a)
  where bit = if _ then 1 else 0
