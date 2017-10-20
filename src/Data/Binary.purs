module Data.Binary
  ( module Bit
  , class Binary
  , class Elastic
  , class Fixed
  , class FitsInt
  , Nibble(..)
  , Byte(..)
  , Overflow(..)
  , discardOverflow
  , _0
  , _1
  , half
  , double
  , isOdd
  , isEven
  , invert
  , add'
  , add
  , unsafeAdd
  , leftShift
  , unsafeLeftShift
  , rightShift
  , unsafeRightShift
  , toBinString
  , tryFromBinString
  , tryFromBinStringElastic
  , toInt
  , tryToInt
  , fromInt
  , tryFromInt
  , toBits
  , fromBits
  , tryFromBits
  , numBits
  , extendOverflow
  ) where

import Conditional (ifelse)
import Data.Array ((:))
import Data.Array as A
import Data.Bit (Bit(..), bitToChar, charToBit)
import Data.Bit as Bit
import Data.Foldable (foldl, foldr, length)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.String (fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (get1)
import Prelude hiding (add)
import Type.Proxy (Proxy)


class Ord a <= Binary a where
  _0 :: a
  _1 :: a
  invert :: a -> a
  add' :: Bit -> a -> a -> Overflow Bit a
  leftShift :: Bit -> a -> Overflow Bit a
  rightShift :: Bit -> a -> Overflow Bit a
  toBits :: a -> Array Bit

half :: ∀ a. Binary a => a -> a
half = rightShift _0 >>> discardOverflow

double :: ∀ a. Binary a => a -> a
double = leftShift _0 >>> discardOverflow

isOdd :: ∀ a. Binary a => a -> Boolean
isOdd = toBits >>> A.last >>> maybe false (eq _1)

isEven :: ∀ a. Binary a => a -> Boolean
isEven = toBits >>> A.last >>> maybe true (eq _0)

toBinString :: ∀ a. Binary a => a -> String
toBinString = toBits >>> map bitToChar >>> fromCharArray

tryFromBinString :: ∀ a. Fixed a => String -> Maybe a
tryFromBinString =
  toCharArray >>> traverse charToBit >=> tryFromBits

diff :: ∀ a. Elastic a => a -> a -> a
diff a b | a == b =  _0
diff a b | a < b = diff b a
diff a b = fromBits bits where
  bits :: Array Bit
  bits = let (Tuple _ acc) = foldr f (Tuple false []) pairs in acc
  f :: (Tuple Bit Bit) -> Tuple Boolean (Array Bit) -> Tuple Boolean (Array Bit)
  f (Tuple (Bit false) (Bit false)) (Tuple false rs) = Tuple false (_0 : rs)
  f (Tuple (Bit false) (Bit true) ) (Tuple false rs) = Tuple true  (_1 : rs)
  f (Tuple (Bit true)  (Bit false)) (Tuple false rs) = Tuple false (_1 : rs)
  f (Tuple (Bit true)  (Bit true) ) (Tuple false rs) = Tuple false (_0 : rs)
  f (Tuple (Bit false) (Bit false)) (Tuple true rs)  = Tuple true  (_1 : rs)
  f (Tuple (Bit false) (Bit true) ) (Tuple true rs)  = Tuple true  (_0 : rs)
  f (Tuple (Bit true)  (Bit false)) (Tuple true rs)  = Tuple false (_0 : rs)
  f (Tuple (Bit true)  (Bit true) ) (Tuple true rs)  = Tuple true  (_0 : rs)
  pairs = A.zip as bs
  as = toBits a
  bs = toBits b

divMod :: ∀ a. Elastic a => a -> a -> Tuple a a
divMod x _ | x == _0 = Tuple _0 _0
divMod x y =
  let (Tuple q r) = divMod (half x) y
      q0 = double q
      r0 = double r
      r1 = if isOdd r0 then unsafeAdd r0 _1 else r0
  in if r1 >= y
     then Tuple (unsafeAdd q0 _1) (diff r1 y)
     else Tuple q0 r1

class Binary a <= FitsInt a where
  toInt :: a -> Int

class Binary a <= Fixed a where
  numBits :: Proxy a -> Int
  tryFromBits :: Array Bit -> Maybe a

tryToInt :: ∀ a. Binary a => a -> Maybe Int
tryToInt binary | bts <- toBits binary =
  bitsToInt (length bts) bts where
    bitsToInt l _ | l > 31 = Nothing
    bitsToInt 0 _ = Just 0
    bitsToInt _ bits = Just $ get1 $ foldr f (Tuple 0 1) bits
    f b (Tuple r p) = Tuple (p * toInt b + r) (p * 2)

-- | Converts a non-negative `Int` value into an `Array Bit`
intToBits :: Int -> Array Bit
intToBits = intToBits' >>> A.dropWhile (eq _0) where
  intToBits' 0 = _0
  intToBits' n | n `mod` 2 == 1 = (intToBits (n `div` 2)) <> _1
               | otherwise = (intToBits (n `div` 2)) <> _0

tryFromInt :: ∀ a. Fixed a => Int -> Maybe a
tryFromInt = intToBits >>> tryFromBits


class Binary a <= Elastic a where
  fromBits :: Array Bit -> a
  extendOverflow :: Overflow Bit a -> a

tryFromBinStringElastic :: ∀ a. Elastic a => String -> Maybe a
tryFromBinStringElastic =
  toCharArray >>> traverse charToBit >>> map fromBits

fromInt :: ∀ a. Elastic a => Int -> a
fromInt = intToBits >>> fromBits


instance binaryBit :: Binary Bit where
  _0 = Bit false
  _1 = Bit true

  invert (Bit b) = Bit (not b)

  add' (Bit false) (Bit false) (Bit false) = Overflow _0 _0
  add' (Bit false) (Bit false) (Bit true)  = Overflow _0 _1
  add' (Bit false) (Bit true) (Bit false)  = Overflow _0 _1
  add' (Bit false) (Bit true) (Bit true)   = Overflow _1 _0
  add' (Bit true) (Bit false) (Bit false)  = Overflow _0 _1
  add' (Bit true) (Bit false) (Bit true)   = Overflow _1 _0
  add' (Bit true) (Bit true) (Bit false)   = Overflow _1 _0
  add' (Bit true) (Bit true) (Bit true)    = Overflow _1 _1

  toBits = A.singleton

  leftShift b a = Overflow a b
  rightShift b a = Overflow a b

instance fixedBit :: Fixed Bit where
  numBits _ = 1

  tryFromBits [b] = Just b
  tryFromBits _ = Nothing

instance fitsIntBit :: FitsInt Bit where
  toInt (Bit b) = ifelse b 1 0

instance binaryArrayBit :: Binary (Array Bit) where
  _0 = pure _0
  _1 = pure _1

  invert = map invert

  add' bit as bs = foldr f acc pairs where
    f (Tuple a b) (Overflow o t) = flip A.cons t <$> add' o a b
    acc = Overflow bit []
    pairs = A.zip (leftPadZero len as) (leftPadZero len bs)
    len = max (A.length as) (A.length bs)
    leftPadZero :: ∀ a. Binary a => Int -> Array a -> Array a
    leftPadZero l xs | A.length xs < l = leftPadZero l (A.cons _0 xs)
    leftPadZero _ xs = xs

  leftShift bit = foldr f (Overflow bit []) where
    f a (Overflow o t) = flip A.cons t <$> leftShift o a

  rightShift bit = foldl f (Overflow bit []) where
    f (Overflow o t) a = A.snoc t <$> rightShift o a

  toBits = id


data Overflow b a = Overflow b a
derive instance eqOverflow :: (Eq b, Eq a) => Eq (Overflow b a)
instance showOverflow :: (Show b, Show a) => Show (Overflow b a) where
  show (Overflow b a) = show a <> " with " <> show b <> " overflow"

instance functorOverflow :: Functor (Overflow b) where
  map f (Overflow o a) = Overflow o (f a)

discardOverflow :: ∀ b a . Overflow b a -> a
discardOverflow (Overflow _ a) = a


-- | Unsigned binary addition
-- | Returns overflow bit
add :: ∀ a. Binary a => a -> a -> Overflow Bit a
add = add' _0

-- | Unsigned binary addition
-- | Discards overflow bit
unsafeAdd :: ∀ a. Binary a => a -> a -> a
unsafeAdd a1 a2 = discardOverflow (add a1 a2)

unsafeRightShift :: ∀ a. Binary a => a -> a
unsafeRightShift a = discardOverflow (rightShift _0 a)

unsafeLeftShift :: ∀ a. Binary a => a -> a
unsafeLeftShift a = discardOverflow (leftShift _0 a)


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

  invert (Nibble a b c d) = Nibble (invert a) (invert b) (invert c) (invert d)

  -- | Unsigned binary addition
  -- | Accepts a carry-over bit from the previous addition
  -- | Returns resulting `Nibble` with overflow bit
  add' b (Nibble a0 b0 c0 d0) (Nibble a1 b1 c1 d1) =
    let (Overflow d' d) = add' d0 d1 b
        (Overflow c' c) = add' c0 c1 d'
        (Overflow b' b) = add' b0 b1 c'
        (Overflow a' a) = add' a0 a1 b'
    in Overflow a' (Nibble a b c d)

  toBits (Nibble a b c d) = [a, b, c, d]

  leftShift z (Nibble a b c d) = Overflow a (Nibble b c d z)
  rightShift z (Nibble a b c d) = Overflow d (Nibble z a b c)

instance fixedNibble :: Fixed Nibble where
  numBits _ = 4

  tryFromBits = zeroWiden 4 >>> tryFromBits' where
    tryFromBits' [a, b, c, d] = Just (Nibble a b c d)
    tryFromBits' _ = Nothing

instance fitsIntNibble :: FitsInt Nibble where
  toInt (Nibble a b c d) = 8 * bin a + 4 * bin b + 2 * bin c + bin d where
    bin (Bit bt) = ifelse bt 1 0


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
  tryFromBits = zeroWiden 8 >>> tryFromBits' where
    tryFromBits' [a, b, c, d, e, f, g, h] =
      Just (Byte (Nibble a b c d) (Nibble e f g h))
    tryFromBits' _ = Nothing

instance fitsIntByte :: FitsInt Byte where
  toInt (Byte h l) = 16 * toInt h + toInt l

zeroWiden :: Int -> Array Bit -> Array Bit
zeroWiden w bits =
  if d < 1 then bits else (A.replicate d (Bit false)) <> bits
    where d = sub w (A.length bits)
