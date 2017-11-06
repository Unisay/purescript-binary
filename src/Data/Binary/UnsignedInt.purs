module Data.Binary.UnsignedInt
  ( UnsignedInt
  , fromInt
  , toInt
  ) where

import Prelude

import Data.Array ((:))
import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Binary (class Binary, Bits(Bits), Overflow(NoOverflow), _0, _1)
import Data.Binary as Bin
import Data.Binary.BaseN (class BaseN, Radix(Radix), unsafeBitsAsChars)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap)
import Data.String as Str
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Typelevel.Num (class GtEq, class Lt, class Pos, type (:*), D1, D16, D2, D31, D32, D5, D6, D64, D8)
import Data.Typelevel.Num as Nat
import Data.Typelevel.Undefined (undefined)


type Uint8   = UnsignedInt D8
type Uint16  = UnsignedInt D16
type Uint32  = UnsignedInt D32
type Uint64  = UnsignedInt D64
type Uint128 = UnsignedInt (D1 :* D2 :* D8)
type Uint256 = UnsignedInt (D2 :* D5 :* D6)

newtype UnsignedInt b = UnsignedInt Bits

derive instance newtypeUnsignedInt :: Newtype (UnsignedInt b) _

instance eqUnsignedInt :: Pos b => Eq (UnsignedInt b) where
  eq (UnsignedInt bits) (UnsignedInt bits') = eq bits bits'

instance ordUnsignedInt :: Pos b => Ord (UnsignedInt b) where
  compare (UnsignedInt as) (UnsignedInt bs) =
    (uncurry compare) $ bimap unwrap unwrap $ Bin.align as bs

instance showUnsignedInt :: Pos b => Show (UnsignedInt b) where
  show (UnsignedInt bits) =
    "UnsignedInt" <> show (Nat.toInt (undefined :: b)) <> "#" <> Bin.toString bits

-- | Converts `Int` value to `UnsignedInt b` for b >= 31
-- | Behavior for negative `Int` values is unspecified.
fromInt :: ∀ b . Pos b => GtEq b D31 => b -> Int -> UnsignedInt b
fromInt _ i | i < 0 = zero
fromInt _ i = UnsignedInt $ Bin.stripLeadingZeros $ Bin.intToBits i

toInt :: ∀ b . Pos b => Lt b D32 => UnsignedInt b -> Int
toInt ui@(UnsignedInt bits) = Bin.unsafeBitsToInt bits

instance binaryUnsignedInt :: Pos b => Binary (UnsignedInt b) where
  msb (UnsignedInt bits) = Bin.msb bits
  lsb (UnsignedInt bits) = Bin.lsb bits
  and (UnsignedInt as) (UnsignedInt bs) = UnsignedInt (Bin.and as bs)
  xor (UnsignedInt as) (UnsignedInt bs) = UnsignedInt (Bin.xor as bs)
  or  (UnsignedInt as) (UnsignedInt bs) = UnsignedInt (Bin.or  as bs)
  not (UnsignedInt bs) = UnsignedInt (Bin.not bs)
  leftShift bit (UnsignedInt bs) = UnsignedInt <$> Bin.leftShift bit bs
  rightShift bit (UnsignedInt bs) = UnsignedInt <$> Bin.rightShift bit bs
  toBits (UnsignedInt bs) = Bin.addLeadingZeros (Nat.toInt (undefined :: b)) bs
  tryFromBits bits = f (Bin.stripLeadingZeros bits) where
    f bs | Bin.length bs > Nat.toInt (undefined :: b) = Nothing
    f bs = Just $ UnsignedInt bs

instance boundedUnsignedInt :: Pos b => Bounded (UnsignedInt b) where
  bottom = zero
  top = UnsignedInt (Bits (A.replicate (Nat.toInt (undefined :: b)) _1))

instance semiringUnsignedInt :: Pos b => Semiring (UnsignedInt b) where
  zero = UnsignedInt Bin.zero
  add (UnsignedInt as) (UnsignedInt bs) = Bin.unsafeFromBits result where
    nBits = Nat.toInt (undefined :: b)
    result = wrapBitsOverflow nBits (Bin.addBits _0 as bs)
    wrapBitsOverflow _ (NoOverflow bits) = bits
    wrapBitsOverflow n res =
      let numValues = Bits (_1 : A.replicate n _0)
      in Bin.tail $ Bin.subtractBits (Bin.extendOverflow res) numValues
  one = UnsignedInt Bin.one
  mul (UnsignedInt as) (UnsignedInt bs) = Bin.unsafeFromBits result where
    nBits = Nat.toInt (undefined :: b)
    rawResult = mulBits as bs
    numValues = Bits $ _1 : (A.replicate nBits _0)
    (Tuple quo rem) = divMod rawResult numValues
    result = if Bin.isOdd quo then Bin.tail rem else rem

half :: Bits -> Bits
half = Bin.rightShift _0 >>> snd

double :: Bits -> Bits
double = Bin.leftShift _0 >>> \(Tuple o (Bits bits)) -> Bits (A.cons o bits)

mulBits :: Bits -> Bits -> Bits
mulBits x _ | Bin.isZero x = Bin.zero
mulBits _ y | Bin.isZero y = Bin.zero
mulBits x y =
  let z = mulBits x (half y)
  in if Bin.isEven y then double z else Bin.addBits' _0 x (double z)

divMod :: Bits -> Bits -> Tuple Bits Bits
divMod x _ | Bin.isZero x = Tuple Bin.zero Bin.zero
divMod x y =
  let inc a = Bin.addBits' _0 a Bin.one
      t = divMod (half x) y
      (Tuple q r) = bimap double double t
      r' = if Bin.isOdd x then inc r else r
  in if (UnsignedInt r' :: UnsignedInt D32) >= UnsignedInt y
     then Tuple (inc q) (r' `Bin.subtractBits` y)
     else Tuple q r'

instance ringUnsignedInt :: Pos b => Ring (UnsignedInt b) where
  sub (UnsignedInt as) (UnsignedInt bs) = UnsignedInt $ Bin.subtractBits as bs

instance baseNUnsignedInt :: Pos b => BaseN (UnsignedInt b) where
  toStringAs (Radix r) (UnsignedInt bits) | r == (Bits [_1, _0]) = Bin.toString bits
  toStringAs (Radix r) (UnsignedInt bs) = Str.fromCharArray (req bs []) where
    req bits acc | (UnsignedInt bits :: UnsignedInt b) < UnsignedInt r =
      unsafeBitsAsChars bits <> acc
    req bits acc =
      let (Tuple quo rem) = bits `divMod` r
      in req quo (unsafeBitsAsChars rem <> acc)

-- TODO: fromStringAs
