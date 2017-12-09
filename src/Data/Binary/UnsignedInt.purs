module Data.Binary.UnsignedInt
  ( UnsignedInt
  , magnitude
  , takeUnsignedInt
  , fromInt
  , toInt
  , asBits
  , tryAsBits
  , divModUnsigned
  ) where

import Prelude

import Data.Array ((:))
import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Binary (class Binary, Bits(Bits), Overflow(NoOverflow), _0, _1, tryFromBits)
import Data.Binary as Bin
import Data.Binary.BaseN (class BaseN, Radix(Hex, Oct, Bin))
import Data.Binary.BaseN as Base
import Data.List ((!!))
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.String as Str
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Typelevel.Num (class Gt, class GtEq, class Lt, class Pos, type (:*), D1, D16, D2, D31, D32, D5, D6, D64, D8)
import Data.Typelevel.Num as Nat
import Data.Typelevel.Undefined (undefined)
import Partial.Unsafe (unsafePartial)


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
    "UnsignedInt" <> show (Nat.toInt (undefined :: b)) <> "#" <> Bin.toBinString bits

magnitude :: ∀ b. UnsignedInt b -> b
magnitude _ = undefined

takeUnsignedInt :: ∀ b . Pos b => Bits -> UnsignedInt b
takeUnsignedInt = Bin.take b >>> Bin.stripLeadingZeros >>> UnsignedInt
  where b = Nat.toInt (undefined :: b)

-- | Converts `Int` value to `UnsignedInt b` for b >= 31
-- | Behavior for negative `Int` values is unspecified.
fromInt :: ∀ b . Pos b => GtEq b D31 => b -> Int -> UnsignedInt b
fromInt _ i | i < 0 = zero
fromInt _ i = UnsignedInt $ Bin.stripLeadingZeros $ Bin.intToBits i

toInt :: ∀ b . Pos b => Lt b D32 => UnsignedInt b -> Int
toInt ui@(UnsignedInt bits) = Bin.unsafeBitsToInt bits

asBits :: ∀ a b . Pos a => Pos b => Lt a b => UnsignedInt a -> UnsignedInt b
asBits (UnsignedInt a) = UnsignedInt a

tryAsBits :: ∀ a b . Pos a => Pos b => Gt a b => UnsignedInt a -> Maybe (UnsignedInt b)
tryAsBits (UnsignedInt a) = Bin.tryFromBits a

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
      in Bin.tail $ subtractBits (Bin.extendOverflow res) numValues
  one = UnsignedInt Bin.one
  mul (UnsignedInt as) (UnsignedInt bs) = Bin.unsafeFromBits result where
    nBits = Nat.toInt (undefined :: b)
    rawResult = mulBits as bs
    numValues = Bits $ _1 : (A.replicate nBits _0)
    (Tuple quo rem) = divModUnsigned rawResult numValues
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

subtractBits :: Bits -> Bits -> Bits
subtractBits as bs = uncurry Bin.subtractBits $ Bin.align as bs

-- Only for positive representations
divModUnsigned :: Bits -> Bits -> Tuple Bits Bits
divModUnsigned x _ | Bin.isZero x = Tuple x x
divModUnsigned x y =
  let t = divModUnsigned (half x) y
      (Tuple q r) = bimap double double t
      r' = if Bin.isOdd x then increment r else r
  in if (UnsignedInt r' :: UnsignedInt D32) >= UnsignedInt y
     then Tuple (increment q) (r' `subtractBits` y)
     else Tuple q r'

increment :: Bits -> Bits
increment a = Bin.discardOverflow $ Bin.addBits _1 Bin.zero a

instance ringUnsignedInt :: Pos b => Ring (UnsignedInt b) where
  sub (UnsignedInt as) (UnsignedInt bs) = UnsignedInt $ subtractBits as bs


instance baseNUnsignedInt :: Pos b => BaseN (UnsignedInt b) where
  toStringAs Bin (UnsignedInt bits) = Bin.toBinString bits
  toStringAs Oct (UnsignedInt bits) = Bin.toOctString bits
  toStringAs Hex (UnsignedInt bits) = Bin.toHexString bits
  toStringAs r (UnsignedInt bs) = Str.fromCharArray (req bs []) where -- TODO better decimal converion using doubling method
    req bits acc | (UnsignedInt bits :: UnsignedInt b) < UnsignedInt (Base.toBits r) =
      unsafeAsChars bits <> acc
    req bits acc =
      let (Tuple quo rem) = bits `divModUnsigned` (Base.toBits r)
      in req quo (unsafeAsChars rem <> acc)
    unsafeAsChars bb = A.singleton $ unsafePartial $ fromJust $ chars !! Bin.unsafeBitsToInt bb
    chars = Map.keys (Base.alphabet r)

  fromStringAs _ "" = Nothing
  fromStringAs Bin s = Bin.fromBinString s >>= tryFromBits
  fromStringAs Oct s = Bin.fromOctString s >>= tryFromBits
  fromStringAs Hex s = Bin.fromHexString s >>= tryFromBits
  fromStringAs radix str = fst <$> A.foldr f (Tuple zero one) <$> traverse t cs where
    f i (Tuple r p) = Tuple (p * i + r) (p * base)
    t = charToBits radix >=> Bin.tryFromBits
    cs = Str.toCharArray (Str.toLower str)
    charToBits r = flip Map.lookup (Base.alphabet radix)
    base = UnsignedInt (Base.toBits radix)
