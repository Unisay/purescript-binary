module Data.Binary
  ( class Binary
  , class Elastic
  , class Fixed
  , class FitsInt
  , Bit(..)
  , Bits(..)
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
  , bitsArray
  , intToBits
  , extendOverflow
  ) where

import Conditional (ifelse)
import Control.Applicative (pure, (<#>))
import Control.Apply ((<$>), (<*>))
import Control.Bind ((>=>))
import Control.Plus (empty)
import Data.Array (catMaybes, replicate, (:))
import Data.Array as A
import Data.Boolean (otherwise)
import Data.BooleanAlgebra (class BooleanAlgebra, class HeytingAlgebra, not, (&&))
import Data.Bounded (class Bounded, bottom, top)
import Data.Eq (class Eq, (==))
import Data.EuclideanRing (class Semiring, div, mod, sub, (-))
import Data.Foldable (class Foldable, foldMap, foldl, foldr, length)
import Data.Function (flip, id, (#), ($), (>>>))
import Data.Functor (class Functor, map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..), fromNonEmpty, (:|))
import Data.Ord (class Ord, compare, max, (<), (>))
import Data.Semigroup (class Semigroup, append, (<>))
import Data.Semiring ((*), (+), zero)
import Data.Show (class Show, show)
import Data.String (fromCharArray, toCharArray)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (get1)
import Data.Unfoldable (unfoldr)
import Prelude (unit)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


newtype Bit = Bit Boolean
derive instance newtypeBit :: Newtype Bit _
derive newtype instance eqBit :: Eq Bit
derive newtype instance ordBit :: Ord Bit
derive newtype instance heytingAlgebraBit :: HeytingAlgebra Bit
derive newtype instance booleanAlgebraBit :: BooleanAlgebra Bit
derive newtype instance boundedBit :: Bounded Bit
instance showBit :: Show Bit where show = toBinString

newtype Bits = Bits (NonEmpty Array Bit)
derive instance newtypeBits :: Newtype Bits _
derive newtype instance eqBits :: Eq Bits
derive newtype instance ordBits :: Ord Bits
instance semigroupBits :: Semigroup Bits where
  append (Bits (NonEmpty a as)) (Bits nbs) = Bits (a :| as :| bitsArray nbs)

instance binaryBits :: Binary Bits where
  invert (Bits bs) = Bits (map invert bs)
  add' bit (Bits (NonEmpty a as)) (Bits (NonEmpty b bs)) =
    let (Overflow x xs) = (add' bit as bs)
        (Overflow y z) = add' x a b
    in Bits (y :| z :| xs)
  leftShift bit (Bits (NonEmpty a as)) =
    let (Overflow x xs) = leftShift bit as
    in (Overflow a (Bits (x :| xs)))
  rightShift bit (Bits (NonEmpty a as)) =
    let (Overflow x xs) = rightShift a as
        (Overflow y zs) = rightShift bit xs
    in (Overflow x (Bits y :| zs))

  toBits = id

bitsArray :: Bits -> Array Bit
bitsArray (Bits (NonEmpty b bs)) = b : bs

bitsLength :: Bits -> Int
bitsLength = unwrap >>> length

zeroWiden :: Int -> Bits -> Bits
zeroWiden w bits =
  if d < 1 then bits else (zero :| replicate (sub d 1) zero) <> bits
    where d = sub w (bitsLength bits)

class Semiring a <= Binary a where
  invert :: a -> a
  add' :: Bit -> a -> a -> Overflow Bit a
  leftShift :: Bit -> a -> Overflow Bit a
  rightShift :: Bit -> a -> Overflow Bit a
  toBits :: a -> Bits

multiply :: ∀ a. Binary a => a -> a -> Overflow Bit a
multiply md mr = ?x where
  fPartialProducts = unwrap <$> catMaybes (map A.last partialProducts)
  partialProducts :: Array (Array Bit)
  partialProducts = unfoldr f s
  s :: Tuple (Array Bit) Bits
  s = Tuple (bitsArray $ toBits md) (toBits mr)
  f :: Tuple (Array Bit) Bits -> Maybe (Tuple Bits (Tuple (Array Bit) Bits))
  f (Tuple [] mdr) = Nothing
  f (Tuple mdb mdr) =
    let shrinked :: Maybe (Array Bit)
        shrinked = A.tail mdb
        shifted :: Array Bit
        shifted = A.snoc (mdr) zero
    in shrinked <#> \shr -> (Tuple shifted (Tuple shr shifted))


toBinString :: ∀ a. Binary a => a -> String
toBinString = toBits >>> map bitToChar >>> fromCharArray

tryFromBinString :: ∀ a. Fixed a => String -> Maybe a
tryFromBinString = toCharArray >>> traverse charToBit >=> tryFromBits

class Binary a <= FitsInt a where
  toInt :: a -> Int

class Binary a <= Fixed a where
  numBits :: Proxy a -> Int
  tryFromBits :: Bits -> Maybe a

tryToInt :: ∀ a. Binary a => a -> Maybe Int
tryToInt binary | (Bits bts) <- toBits binary =
  bitsToInt (length bts) bts where
    bitsToInt l _ | l > 31 = Nothing
    bitsToInt 0 _ = Just 0
    bitsToInt _ bts = Just $ get1 $ foldr f (Tuple 0 1) bts
    f b (Tuple r p) = Tuple (p * toInt b + r) (p * 2)

tryFromInt :: ∀ a. Fixed a => Int -> Maybe a
tryFromInt = intToBits >>> tryFromBits

class Binary a <= Elastic a where
  fromBits :: Array Bit -> a
  extendOverflow :: Overflow Bit a -> a

tryFromBinStringElastic :: ∀ a. Elastic a => String -> Maybe a
tryFromBinStringElastic = toCharArray >>> traverse charToBit >>> map fromBits

fromInt :: ∀ a. Elastic a => Int -> a
fromInt = intToBits >>> fromBits

-- | Converts a non-negative `Int` value into an `Array Bit`
intToBits :: Int -> Bits
intToBits 0 = Bits (zero :| empty)
intToBits n | n `mod` 2 == 1 = (intToBits (n `div` 2)) <> one
            | otherwise = (intToBits (n `div` 2)) <> zero

leftPadZero :: ∀ a. Binary a => Int -> Array a -> Array a
leftPadZero l xs | A.length xs < l = leftPadZero l (A.cons zero xs)
leftPadZero _ xs = xs

instance semiringBit :: Semiring Bit where
  zero = Bit false
  one = Bit true
  add = unsafeAdd
  mul (Bit a) (Bit b) = Bit (a && b)

instance binaryBit :: Binary Bit where
  invert (Bit b) = Bit (not b)

  add' (Bit false) (Bit false) (Bit false) = Overflow Bit zero zero
  add' (Bit false) (Bit false) (Bit true)  = Overflow Bit zero one
  add' (Bit false) (Bit true) (Bit false)  = Overflow Bit zero one
  add' (Bit false) (Bit true) (Bit true)   = Overflow Bit one zero
  add' (Bit true) (Bit false) (Bit false)  = Overflow Bit zero one
  add' (Bit true) (Bit false) (Bit true)   = Overflow Bit one zero
  add' (Bit true) (Bit true) (Bit false)   = Overflow Bit one zero
  add' (Bit true) (Bit true) (Bit true)    = Overflow Bit one one

  toBits = pure

  leftShift b a = Overflow Bit a b
  rightShift b a = Overflow Bit a b

instance fixedBit :: Fixed Bit where
  numBits _ = 1

  tryFromBits (Bits (NonEmpty b [])) = Just b
  tryFromBits _ = Nothing

instance fitsIntBit :: FitsInt Bit where
  toInt (Bit b) = ifelse b 1 0


data Overflow b a = Overflow b a
derive instance eqOverflow :: (Eq b, Eq a) => Eq (Overflow b a)
instance showOverflow :: (Show b, Show a) => Show (Overflow b a) where
  show (Overflow b a) = show a <> " with " <> show b <> " overflow"

instance functorOverflow :: Functor (Overflow b) where
  map f (Overflow o a) = Overflow o (f a)

discardOverflow :: ∀ b a . Overflow b a -> a
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
add :: ∀ a. Binary a => a -> a -> Overflow Bit a
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

instance boundedNibble :: Bounded Nibble where
  top = Nibble top top top top
  bottom = Nibble bottom bottom bottom bottom

instance semiringNibble :: Semiring Nibble where
  zero = Nibble zero zero zero zero
  one = Nibble zero zero zero one
  add = unsafeAdd
  mul = unsafeCoerce unit

instance binaryNibble :: Binary Nibble where
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

instance semiringByte :: Semiring Byte where
  zero = Byte zero zero
  one = Byte zero one
  add = unsafeAdd
  mul = unsafeCoerce unit

instance binaryByte :: Binary Byte where
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
    tryFromBits' [a, b, c, d, e, f, g, h] = Just (Byte (Nibble a b c d) (Nibble e f g h))
    tryFromBits' _ = Nothing

instance fitsIntByte :: FitsInt Byte where
  toInt (Byte h l) = 16 * toInt h + toInt l
