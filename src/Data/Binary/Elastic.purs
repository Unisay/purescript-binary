module Data.Binary.Elastic
  ( class Elastic
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
  ) where

import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Binary (class Binary, _0, _1, add, isOdd, leftShift, rightShift, toBits)
import Data.Binary.Bit (Bit(..), charToBit)
import Data.Binary.Bits (Bits(..), align, intToBits)
import Data.Binary.Overflow (Overflow(..), discardOverflow)
import Data.Foldable (foldr)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Data.String as Str
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Prelude hiding (add)

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
  bits = let (Tuple _ acc) = foldr f (Tuple false []) pairs in Bits acc
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
