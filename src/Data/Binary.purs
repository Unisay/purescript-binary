module Data.Binary
  ( class Binary
  , class Elastic
  , class Fixed
  , Bit(..)
  , Nibble(..)
  , Byte(..)
  , Overflow(..)
  , discardOverflow
  , bitToChar
  , charToBit
  , invert
  , add'
  , add
  , unsafeAdd
  , zero
  , one
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
  , intToBitArray
  ) where

import Data.Array as A
import Conditional (ifelse)
import Control.Applicative (pure)
import Control.Apply ((<$>), (<*>))
import Control.Bind ((>=>))
import Data.Array (foldMap, foldl, foldr)
import Data.Boolean (otherwise)
import Data.BooleanAlgebra (class BooleanAlgebra, not)
import Data.Bounded (class Bounded)
import Data.Eq (class Eq, (==))
import Data.EuclideanRing (div, mod)
import Data.Function (flip, ($), (>>>))
import Data.Functor (class Functor, map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (class Ord, compare, max, (<), (>))
import Data.Semigroup ((<>))
import Data.Semiring ((*), (+))
import Data.Show (class Show, show)
import Data.String (fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (get1)


newtype Bit = Bit Boolean
derive newtype instance eqBit :: Eq Bit
derive newtype instance ordBit :: Ord Bit
derive newtype instance booleanAlgebraBit :: BooleanAlgebra Bit
derive newtype instance boundedBit :: Bounded Bit
instance showBit :: Show Bit where show = toBinString

class Binary a where
  invert :: a -> a
  add' :: Bit -> a -> a -> Overflow a
  zero :: a
  leftShift :: Bit -> a -> Overflow a
  rightShift :: Bit -> a -> Overflow a
  toBits :: a -> Array Bit

toBinString :: ∀ a. Binary a => a -> String
toBinString = toBits >>> map bitToChar >>> fromCharArray

tryFromBinString :: ∀ a. Fixed a => String -> Maybe a
tryFromBinString = toCharArray >>> traverse charToBit >=> tryFromBits

class Binary a <= Fixed a where
  tryFromBits :: Array Bit -> Maybe a
  toInt :: a -> Int

tryFromInt :: ∀ a. Fixed a => Int -> Maybe a
tryFromInt = intToBitArray >>> tryFromBits


class Binary a <= Elastic a where
  fromBits :: Array Bit -> a
  tryToInt :: a -> Maybe Int

tryFromBinStringElastic :: ∀ a. Elastic a => String -> Maybe a
tryFromBinStringElastic = toCharArray >>> traverse charToBit >>> map fromBits

fromInt :: ∀ a. Elastic a => Int -> a
fromInt = intToBitArray >>> fromBits

-- | Converts a non-negative `Int` value into an `Array Bit`
intToBitArray :: Int -> Array Bit
intToBitArray 0 = []
intToBitArray n | n `mod` 2 == 1 = A.snoc (intToBitArray (n `div` 2)) one
                | otherwise = A.snoc (intToBitArray (n `div` 2)) zero

leftPadZero :: ∀ a. Binary a => Int -> Array a -> Array a
leftPadZero l xs | A.length xs < l = leftPadZero l (A.cons zero xs)
leftPadZero _ xs = xs

instance binaryBit :: Binary Bit where
  invert (Bit b) = Bit (not b)

  add' (Bit false) (Bit false) (Bit false) = Overflow zero zero
  add' (Bit false) (Bit false) (Bit true)  = Overflow zero one
  add' (Bit false) (Bit true) (Bit false)  = Overflow zero one
  add' (Bit false) (Bit true) (Bit true)   = Overflow one zero
  add' (Bit true) (Bit false) (Bit false)  = Overflow zero one
  add' (Bit true) (Bit false) (Bit true)   = Overflow one zero
  add' (Bit true) (Bit true) (Bit false)   = Overflow one zero
  add' (Bit true) (Bit true) (Bit true)    = Overflow one one

  zero = Bit false

  toBits = pure

  leftShift b a = Overflow a b
  rightShift b a = Overflow a b

instance fixedBit :: Fixed Bit where
  tryFromBits [b] = Just b
  tryFromBits _ = Nothing

  toInt (Bit b) = ifelse b 1 0


data Overflow a = Overflow Bit a
derive instance eqOverflow :: Eq a => Eq (Overflow a)
instance showOverflow :: Show a => Show (Overflow a) where
  show (Overflow (Bit true) b) = show b <> " with one bit overflow"
  show (Overflow (Bit false) b) = show b <> " without overflow"

instance functorOverflow :: Functor Overflow where
  map f (Overflow o a) = Overflow o (f a)

discardOverflow :: ∀ a. Overflow a -> a
discardOverflow (Overflow _ a) = a

charToBit :: Char -> Maybe Bit
charToBit '1' = Just one
charToBit '0' = Just zero
charToBit _   = Nothing

bitToChar :: Bit -> Char
bitToChar (Bit true)  = '1'
bitToChar (Bit false) = '0'

-- | Unsigned binary addition
-- | Returns overflow bit
add :: ∀ a. Binary a => a -> a -> Overflow a
add = add' zero

-- | Unsigned binary addition
-- | Discards overflow bit
unsafeAdd :: ∀ a. Binary a => a -> a -> a
unsafeAdd a1 a2 = discardOverflow (add a1 a2)

unsafeRightShift :: ∀ a. Binary a => a -> a
unsafeRightShift a = discardOverflow (rightShift zero a)

unsafeLeftShift :: ∀ a. Binary a => a -> a
unsafeLeftShift a = discardOverflow (leftShift zero a)

one :: ∀ a. Binary a => a
one = discardOverflow (leftShift (Bit true) zero)

data Nibble = Nibble Bit Bit Bit Bit

derive instance eqNibble :: Eq Nibble
instance ordNibble :: Ord Nibble where
  compare a b = compare (toBits a) (toBits b)

instance showNibble :: Show Nibble where
  show = toBinString

instance bitsNibble :: Binary Nibble where
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

  zero = Nibble zero zero zero zero

  toBits (Nibble a b c d) = [a, b, c, d]

  leftShift z (Nibble a b c d) = Overflow a (Nibble b c d z)
  rightShift z (Nibble a b c d) = Overflow d (Nibble z a b c)

instance fixedNibble :: Fixed Nibble where
  tryFromBits = leftPadZero 4 >>> tryFromBits' where
    tryFromBits' [a, b, c, d] = Just (Nibble a b c d)
    tryFromBits' _ = Nothing

  toInt (Nibble a b c d) = 8 * bin a + 4 * bin b + 2 * bin c + bin d where
    bin (Bit bt) = ifelse bt 1 0


data Byte = Byte Nibble Nibble

derive instance eqByte :: Eq Byte

instance showByte :: Show Byte where
 show = toBinString

instance bitsByte :: Binary Byte where
  invert (Byte n1 n2) = Byte (invert n1) (invert n2)

  zero = Byte zero zero
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
  tryFromBits = leftPadZero 8 >>> tryFromBits' where
    tryFromBits' [a, b, c, d, e, f, g, h] = Just (Byte (Nibble a b c d) (Nibble e f g h))
    tryFromBits' _ = Nothing

  toInt (Byte h l) = 16 * toInt h + toInt l


instance binaryArrayByte :: Binary (Array Byte) where
  invert = map invert
  zero = pure zero
  toBits = foldMap toBits

  add' bit as bs = foldr f acc pairs where
    f (Tuple a b) (Overflow o t) = flip A.cons t <$> add' o a b
    acc = Overflow bit []
    pairs = A.zip (leftPadZero len as) (leftPadZero len bs)
    len = max (A.length as) (A.length bs)

  leftShift bit = foldr f (Overflow bit []) where
    f a (Overflow o t) = flip A.cons t <$> leftShift o a

  rightShift bit = foldl f (Overflow bit []) where
    f (Overflow o t) a = A.snoc t <$> rightShift o a

instance elasticArrayByte :: Elastic (Array Byte) where
  fromBits bits = fromMaybe [] (parseBytes pbits) where
    nbytes = A.length bits `div` 8
    pbits = leftPadZero (nbytes * 8) bits
    parseBytes [] = Just []
    parseBytes xs = A.cons <$> tryFromBits (A.take 8 xs) <*> parseBytes (A.drop 8 xs)

  tryToInt bytes = tryToInt' (A.length bytes) bytes where
    tryToInt' l _ | l > 4 = Nothing
    tryToInt' 4 [(Byte (Nibble (Bit true) _ _ _) _), _] = Nothing -- not using 2's complement
    tryToInt' l bs = Just $ get1 $ foldr f (Tuple 0 1) (toBits bs)
    f b (Tuple r p) = Tuple (p * 2) (p * toInt b + r)
