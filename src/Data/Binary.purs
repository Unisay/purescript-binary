module Data.Binary
  ( class Binary
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
  , fromBinString
  , toIntPos
  , toInt
  , fromInt
  , intToBitArray
  ) where

import Data.Array as A
import Data.String as Str
import Conditional (ifelse)
import Control.Applicative (pure)
import Control.Apply ((<$>), (<*>))
import Control.Bind (bind, (>>=))
import Control.Monad (ifM)
import Data.Array (foldMap, foldl, foldr)
import Data.Boolean (otherwise)
import Data.BooleanAlgebra (not, (&&))
import Data.Eq (class Eq, (==))
import Data.EuclideanRing (div, mod, (-))
import Data.Function (flip, ($), (>>>))
import Data.Functor (class Functor, map)
import Data.Int (pow)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Ord (class Ord, compare, max, (<), (<=), (>=))
import Data.Semigroup ((<>))
import Data.Semiring ((*), (+))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(Tuple))
import Data.Unit (unit)
import Unsafe.Coerce (unsafeCoerce)


newtype Bit = Bit Boolean
derive newtype instance eqBit :: Eq Bit
instance showBit :: Show Bit where
  show = toBinString

class Binary a where
  invert :: a -> a
  add' :: Bit -> a -> a -> Overflow a
  zero :: a
  leftShift :: Bit -> a -> Overflow a
  rightShift :: Bit -> a -> Overflow a
  toIntPos :: Int -> a -> Maybe Int
  fromInt :: Int -> Maybe a
  toBinString :: a -> String
  fromBinString :: String -> Maybe a

toInt :: ∀ a. Binary a => a -> Maybe Int
toInt = toIntPos 0

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

  leftShift b a = Overflow a b
  rightShift b a = Overflow a b

  toBinString = bitToChar >>> Str.singleton
  fromBinString s = ifM (Just $ Str.length s == 1) (Str.charAt 0 s >>= charToBit) Nothing

  toIntPos p (Bit b) | p >= 0 && p <= 32 = Just (pow 2 p *  ifelse b 1 0)
  toIntPos _ _ = Nothing

  fromInt 0 = Just zero
  fromInt 1 = Just one
  fromInt _ = Nothing

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

instance showNibble :: Show Nibble where
  show = toBinString

instance ordNibble :: Ord Nibble where
  compare l r = compare (toBinString l) (toBinString r)

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

  leftShift z (Nibble a b c d) = Overflow a (Nibble b c d z)
  rightShift z (Nibble a b c d) = Overflow d (Nibble z a b c)

  toBinString (Nibble a b c d) =
    toBinString a <> toBinString b <> toBinString c <> toBinString d

  fromBinString s | Str.length s < 5 =
    Just $ Nibble (b 0) (b 1) (b 2) (b 3)
      where b i = fromMaybe zero (Str.charAt i s >>= charToBit)
  fromBinString _ = Nothing

  toIntPos p (Nibble a b c d) =
    let bin (Bit bt) = ifelse bt 1 0
    in Just $ pow 2 (p + 3) * bin a
            + pow 2 (p + 2) * bin b
            + pow 2 (p + 1) * bin c
            + pow 2 p * bin d

  fromInt 0 = Just zero
  fromInt i | [a, b, c, d] <- leftPadZero 4 (intToBitArray i) = Just (Nibble a b c d)
  fromInt _ = Nothing


intToBitArray :: Int -> Array Bit
intToBitArray 0 = []
intToBitArray n | n `mod` 2 == 1 = A.snoc (intToBitArray (n `div` 2)) one
                | otherwise = A.snoc (intToBitArray (n `div` 2)) zero

data Byte = Byte Nibble Nibble

derive instance eqByte :: Eq Byte

instance showByte :: Show Byte where
 show = toBinString

instance bitsByte :: Binary Byte where
  invert (Byte n1 n2) = Byte (invert n1) (invert n2)

  zero = Byte zero zero

  leftShift b (Byte h l) =
    let (Overflow o l') = leftShift b l
        (Overflow o' h') = leftShift o h
    in Overflow o' (Byte h' l')

  rightShift b (Byte h l) =
    let (Overflow o h') = rightShift b h
        (Overflow o' l') = rightShift o l
    in Overflow o' (Byte h' l')

  toBinString (Byte n1 n2) = show n1 <> show n2

  fromBinString s | Str.length s < 5 = Byte zero <$> fromBinString s
  fromBinString s | Str.length s < 9 = do
    { before: h, after: l } <- Str.splitAt (Str.length s - 4) s
    Byte <$> fromBinString h <*> fromBinString l
  fromBinString _ = Nothing

  -- | Unsigned binary addition
  -- | Accepts a carry-over bit from the previous addition
  -- | Returns resulting byte with overflow bit
  add' o (Byte h l) (Byte h' l') =
    let (Overflow o' l'') = add' o l l'
        (Overflow o'' h'') = add' o' h h'
    in Overflow o'' (Byte h'' l'')

  fromInt 0 = Just zero
  fromInt i | [a, b, c, d, e, f, g, h] <- leftPadZero 8 (intToBitArray i) =
    Just (Byte (Nibble a b c d) (Nibble e f g h))
  fromInt _ = Nothing

  toIntPos p (Byte h l) = do
    l' <- toIntPos p l
    h' <- toIntPos (p + 4) h
    pure (l' + h')

notImplemented :: ∀ a . a
notImplemented = unsafeCoerce unit

leftPadZero :: ∀ a. Binary a => Int -> Array a -> Array a
leftPadZero l xs | A.length xs < l = leftPadZero l (A.cons zero xs)
leftPadZero _ xs = xs

instance binaryArrayByte :: Binary (Array Byte) where
  invert = map invert
  zero = pure zero

  add' bit as bs = foldr f acc pairs where
    f (Tuple a b) (Overflow o t) = flip A.cons t <$> add' o a b
    acc = Overflow bit []
    pairs = A.zip (leftPadZero len as) (leftPadZero len bs)
    len = max (A.length as) (A.length bs)

  leftShift bit = foldr f acc where
    acc = Overflow bit []
    f a (Overflow o t) = flip A.cons t <$> leftShift o a

  rightShift bit = foldl f acc where
    acc = Overflow bit []
    f (Overflow o t) a = A.snoc t <$> rightShift o a

  toBinString = foldMap toBinString

  fromBinString s | Str.length s < 9 = A.singleton <$> fromBinString (Str.take 8 s)
  fromBinString s = A.cons <$> fromBinString (Str.take 8 s) <*> fromBinString (Str.drop 8 s)

  -- toIntPos :: Int -> a -> Maybe Int
  toIntPos = notImplemented

  -- fromInt :: Int -> Maybe a
  fromInt = notImplemented
