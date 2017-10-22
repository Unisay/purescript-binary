module Data.Binary.Class
  ( Bit(..)
  , bitToChar
  , charToBit
  , Bits(..)
  , align
  , intToBits
  , class Binary
  , class Fixed
  , class FitsInt
  , _0
  , _1
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
  , toInt
  , tryToInt
  , tryFromInt
  , toBits
  , tryFromBits
  , numBits
  , class Elastic
  , fromBits
  , extendOverflow
  , addLeadingZeros
  , stripLeadingZeros
  , extendAdd
  , tryFromBinStringElastic
  , fromInt
  , half
  , double
  , diff
  , divMod
  , multiply
  ) where

import Conditional (ifelse)
import Control.Plus (empty)
import Data.Array (singleton)
import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Binary.Overflow (Overflow(..), discardOverflow)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String as Str
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), uncurry)
import Data.Tuple.Nested (get1)
import Prelude hiding (add)
import Type.Proxy (Proxy)

newtype Bit = Bit Boolean
derive instance newtypeBit :: Newtype Bit _
derive newtype instance eqBit :: Eq Bit
derive newtype instance ordBit :: Ord Bit
derive newtype instance heytingAlgebraBit :: HeytingAlgebra Bit
derive newtype instance booleanAlgebraBit :: BooleanAlgebra Bit
derive newtype instance boundedBit :: Bounded Bit

instance showBit :: Show Bit where
  show = bitToChar >>> singleton >>> Str.fromCharArray

charToBit :: Char -> Maybe Bit
charToBit '1' = Just (Bit true)
charToBit '0' = Just (Bit false)
charToBit _   = Nothing

bitToChar :: Bit -> Char
bitToChar (Bit true)  = '1'
bitToChar (Bit false) = '0'


newtype Bits = Bits (Array Bit)
derive instance newtypeBits :: Newtype Bits _
derive newtype instance eqBits :: Eq Bits
derive newtype instance showBits :: Show Bits
derive newtype instance semigroupBits :: Semigroup Bits

instance ordBits :: Ord Bits where
  compare a b = uncurry compare $ bimap unwrap unwrap $ align a b

instance semiringBits :: Semiring Bits where
  zero = _0
  add = extendAdd
  one = _1
  mul = multiply

-- | Converts a non-negative `Int` value into an `Bits`
intToBits :: Int -> Bits
intToBits = f >>> A.dropWhile (eq (Bit false)) >>> Bits where
  f 0 = [Bit false]
  f n | n `mod` 2 == 1 = A.snoc (f (n `div` 2)) (Bit true)
      | otherwise = A.snoc (f (n `div` 2)) (Bit false)

-- | align length by adding zeroes from the left
align :: Bits -> Bits -> Tuple Bits Bits
align bas@(Bits as) bbs@(Bits bs) =
  case compare la lb of
  EQ -> Tuple bas bbs
  LT -> Tuple (extend (lb - la) as) bbs
  GT -> Tuple bas (extend (la - lb) bs)
  where la = A.length as
        lb = A.length bs
        extend :: Int -> Array Bit -> Bits
        extend d xs = Bits (A.replicate d (Bit false) <> xs)



class Ord a <= Binary a where
  _0 :: a
  _1 :: a
  invert :: a -> a
  add' :: Bit -> a -> a -> Overflow Bit a
  leftShift :: Bit -> a -> Overflow Bit a
  rightShift :: Bit -> a -> Overflow Bit a
  toBits :: a -> Bits

instance binaryBits :: Binary Bits where
  _0 = Bits (pure _0)
  _1 = Bits (pure _1)

  invert (Bits bits) = Bits (map invert bits)

  add' bit abits@(Bits as) bbits@(Bits bs) =
    Bits <$> A.foldr f acc pairs where
      f (Tuple a b) (Overflow o t) = flip A.cons t <$> add' o a b
      acc = Overflow bit empty
      pairs = uncurry A.zip $ bimap unwrap unwrap $ align abits bbits

  leftShift bit (Bits bits) = Bits <$> A.foldr f (Overflow bit []) bits
    where f a (Overflow o t) = flip A.cons t <$> leftShift o a

  rightShift bit (Bits bits) = Bits <$> A.foldl f (Overflow bit []) bits
    where f (Overflow o t) a = A.snoc t <$> rightShift o a

  toBits = id


isOdd :: ∀ a. Binary a => a -> Boolean
isOdd = toBits >>> unwrap >>> A.last >>> maybe false (eq _1)

isEven :: ∀ a. Binary a => a -> Boolean
isEven = toBits >>> unwrap >>> A.last >>> maybe true (eq _0)

toBinString :: ∀ a. Binary a => a -> String
toBinString = toBits >>> unwrap >>> map bitToChar >>> Str.fromCharArray

tryFromBinString :: ∀ a. Fixed a => String -> Maybe a
tryFromBinString =
  Str.toCharArray >>> traverse charToBit >=> Bits >>> tryFromBits

class Binary a <= FitsInt a where
  toInt :: a -> Int

class Binary a <= Fixed a where
  numBits :: Proxy a -> Int
  tryFromBits :: Bits -> Maybe a

tryToInt :: ∀ a. Binary a => a -> Maybe Int
tryToInt binary | (Bits bts) <- toBits binary =
  bitsToInt (A.length bts) bts where
    bitsToInt l _ | l > 31 = Nothing
    bitsToInt 0 _ = Just 0
    bitsToInt _ bits = Just $ get1 $ A.foldr f (Tuple 0 1) bits
    f b (Tuple r p) = Tuple (p * toInt b + r) (p * 2)

tryFromInt :: ∀ a. Fixed a => Int -> Maybe a
tryFromInt = intToBits >>> tryFromBits


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

  toBits = A.singleton >>> Bits

  leftShift b a = Overflow a b
  rightShift b a = Overflow a b

instance fixedBit :: Fixed Bit where
  numBits _ = 1

  tryFromBits (Bits [b]) = Just b
  tryFromBits _ = Nothing

instance fitsIntBit :: FitsInt Bit where
  toInt (Bit b) = ifelse b 1 0

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



class Binary a <= Elastic a where
  fromBits :: Bits -> a
  extendOverflow :: Overflow Bit a -> a

instance elasticArrayBit :: Elastic Bits where
  fromBits (Bits []) = _0
  fromBits bs = bs
  extendOverflow (Overflow (Bit false) bits) = bits
  extendOverflow (Overflow bit (Bits bits)) = Bits $ A.cons bit bits

addLeadingZeros :: ∀ a. Elastic a => Int -> a -> a
addLeadingZeros w = toBits
                >>> unwrap
                >>> f
                >>> wrap
                >>> fromBits
  where
    f bits = let d = sub w (A.length bits)
             in if d < 1 then bits
                         else (A.replicate d _0) <> bits

stripLeadingZeros :: ∀ a. Elastic a => a -> a
stripLeadingZeros = toBits
                >>> unwrap
                >>> A.dropWhile (eq _0)
                >>> wrap
                >>> fromBits

extendAdd :: ∀ a. Elastic a => a -> a -> a
extendAdd a b = extendOverflow (add a b)

tryFromBinStringElastic :: ∀ a. Elastic a => String -> Maybe a
tryFromBinStringElastic =
  Str.toCharArray >>> traverse charToBit >>> map wrap >>> map fromBits

fromInt :: ∀ a. Elastic a => Int -> a
fromInt = intToBits >>> fromBits

half :: ∀ a. Elastic a => a -> a
half = rightShift _0 >>> discardOverflow >>> stripLeadingZeros

double :: ∀ a. Elastic a => a -> a
double = leftShift _0 >>> extendOverflow >>> stripLeadingZeros

diff :: ∀ a. Elastic a => a -> a -> a
diff a b | a == b =  _0
diff a b | a < b = diff b a
diff a b = fromBits (stripLeadingZeros bits) where
  bits = let (Tuple _ acc) = A.foldr f (Tuple false []) pairs in Bits acc
  f :: (Tuple Bit Bit) -> Tuple Boolean (Array Bit) -> Tuple Boolean (Array Bit)
  -- https://i.stack.imgur.com/5M40R.jpg
  f (Tuple (Bit false) (Bit false)) (Tuple false rs) = Tuple false (A.cons _0 rs)
  f (Tuple (Bit false) (Bit false)) (Tuple true rs)  = Tuple true  (A.cons _1 rs)
  f (Tuple (Bit false) (Bit true) ) (Tuple false rs) = Tuple true  (A.cons _1 rs)
  f (Tuple (Bit false) (Bit true) ) (Tuple true rs)  = Tuple true  (A.cons _0 rs)
  f (Tuple (Bit true)  (Bit false)) (Tuple false rs) = Tuple false (A.cons _1 rs)
  f (Tuple (Bit true)  (Bit false)) (Tuple true rs)  = Tuple false (A.cons _0 rs)
  f (Tuple (Bit true)  (Bit true) ) (Tuple false rs) = Tuple false (A.cons _0 rs)
  f (Tuple (Bit true)  (Bit true) ) (Tuple true rs)  = Tuple true  (A.cons _1 rs)
  pairs = uncurry A.zip $ bimap unwrap unwrap $ align (toBits a) (toBits b)

divMod :: ∀ a. Elastic a => a -> a -> Tuple a a
divMod x _ | x == _0 = Tuple _0 _0
divMod x y = if r' >= y
             then Tuple (inc q) (r' `diff` y)
             else Tuple q r'
  where
    t = divMod (half x) y
    (Tuple q r) = bimap double double t
    r' = if isOdd x then inc r else r
    inc a = extendAdd a _1

multiply :: ∀ a. Elastic a => a -> a -> a
multiply x _ | x == _0 = _0
multiply _ y | y == _0 = _0
multiply x y = let z = multiply x (half y)
               in if isEven y then double z
                              else extendAdd x (double z)
