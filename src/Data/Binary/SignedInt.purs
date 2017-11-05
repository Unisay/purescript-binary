module Data.Binary.SignedInt
  ( SignedInt
  , fromInt
  , toInt
  , isNegative
  , complement
  , flipSign
  ) where

import Prelude

import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Binary (class Binary, Bit, Bits(Bits), Overflow(NoOverflow), _0, _1)
import Data.Binary as Bin
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (abs)
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Typelevel.Num (class GtEq, class LtEq, type (:*), D1, D16, D2, D32, D5, D6, D64, D8)
import Data.Typelevel.Num as Nat
import Data.Typelevel.Num.Sets (class Pos)
import Data.Typelevel.Undefined (undefined)


type Int8   = SignedInt D8
type Int16  = SignedInt D16
type Int32  = SignedInt D32
type Int64  = SignedInt D64
type Int128 = SignedInt (D1 :* D2 :* D8)
type Int256 = SignedInt (D2 :* D5 :* D6)

newtype SignedInt b = SignedInt Bits

derive instance newtypeSignedInt :: Newtype (SignedInt b) _

instance eqSignedInt :: Pos b => Eq (SignedInt b) where
  eq (SignedInt bits) (SignedInt bits') = eq bits bits'

instance ordSignedInt :: Pos b => Ord (SignedInt b) where
  compare a b | isNegative a && not (isNegative b) = LT
  compare a b | not (isNegative a) && isNegative b = GT
  compare a@(SignedInt as) b@(SignedInt bs) =
    f (isNegative a) (isNegative b) where
      f true true = cmp
      f false false = cmp
      f true false = LT
      f false true = GT
      cmp = uncurry compare $ bimap unwrap unwrap $ Bin.align as bs

instance showSignedInt :: Pos b => Show (SignedInt b) where
  show (SignedInt bits) =
    "SignedInt" <> show (Nat.toInt (undefined :: b)) <> "#" <> Bin.toBinString bits

flipSign :: ∀ b. Pos b => SignedInt b -> SignedInt b
flipSign (SignedInt bits) =
  let { head: h, tail: (Bits t) } = Bin.uncons bits
      bs = Bits $ A.cons (Bin.not h) t
  in SignedInt bs

unsafeIncrement :: Bits -> Bits
unsafeIncrement = Bin.discardOverflow <<< Bin.addBits _1 _0

complementBits :: Bits -> Bits
complementBits = Bin.not >>> unsafeIncrement

complement :: ∀ b . SignedInt b -> SignedInt b
complement = unwrap >>> complementBits >>> SignedInt

-- | Converts `Int` value to `SignedInt b` for b >= 31
fromInt :: ∀ b . Pos b => GtEq b D32 => b -> Int -> SignedInt b
fromInt b i = SignedInt signed where
  signed = twosComplement (Nat.toInt b) (i < 0) (Bin.intToBits (abs i))
  twosComplement :: Int -> Boolean -> Bits -> Bits
  twosComplement w false bits = Bin.addLeadingZeros w bits
  twosComplement w true bits@(Bits bs) =
    case compare (A.length bs) (w - 1) of
    GT -> bits
    EQ -> complementBits bits
    LT -> complementBits (Bin.addLeadingZeros (w - 1) bits)

toInt :: ∀ b . Pos b => LtEq b D32 => SignedInt b -> Int
toInt si | si == top = top
toInt si | si == bottom = bottom
toInt si@(SignedInt bits) =
  if isNegative si
  then negate let (SignedInt bb) = complement si in bitsToInt bb
  else bitsToInt bits
  where bitsToInt = Bin.tail >>> Bin.unsafeBitsToInt

isNegative :: ∀ b . SignedInt b -> Boolean
isNegative (SignedInt bits) = Bin.head bits == _1

instance binarySignedInt :: Pos b => Binary (SignedInt b) where
  _0 = SignedInt _0
  _1 = SignedInt _1
  msb (SignedInt bits) = Bin.msb bits
  lsb (SignedInt bits) = Bin.lsb bits
  and (SignedInt as) (SignedInt bs) = SignedInt (Bin.and as bs)
  xor (SignedInt as) (SignedInt bs) = SignedInt (Bin.xor as bs)
  or  (SignedInt as) (SignedInt bs) = SignedInt (Bin.or  as bs)
  not (SignedInt bs) = SignedInt (Bin.not bs)
  leftShift bit (SignedInt bs) = SignedInt <$> Bin.leftShift bit bs
  rightShift bit (SignedInt bs) = SignedInt <$> Bin.rightShift bit bs
  toBits (SignedInt bs) = bs
  tryFromBits bits =
    case compare (Bin.length bits) width of
      EQ -> Just (SignedInt bits)
      LT -> Just (SignedInt $ Bin.addLeadingZeros width bits)
      GT -> Nothing
    where width = Nat.toInt (undefined :: b)

instance boundedSignedInt :: Pos b => Bounded (SignedInt b) where
  bottom = SignedInt (Bits (A.cons _1 (A.replicate (Nat.toInt (undefined :: b) - 1) _0)))
  top    = SignedInt (Bits (A.cons _0 (A.replicate (Nat.toInt (undefined :: b) - 1) _1)))

signExtend :: Int -> Bits -> Bits
signExtend width bits | Bin.head bits == _0 = Bin.addLeadingZeros width bits
signExtend width (Bits bits) =
  let d = sub width (A.length bits)
  in Bits if d < 1 then bits else (A.replicate d _1) <> bits

instance semiringSignedInt :: Pos b => Semiring (SignedInt b) where
  zero = _0
  add (SignedInt as) (SignedInt bs) = Bin.unsafeFromBits result where
    nBits = Nat.toInt (undefined :: b)
    result = wrapBitsOverflow nBits (Bin.addBits _0 as bs)
    wrapBitsOverflow _ (NoOverflow bits) = bits
    wrapBitsOverflow n res =
      let numValues = _1 <> Bits (A.replicate n _0)
      in Bin.tail $ Bin.subtractBits (Bin.extendOverflow res) numValues
  one = _1
  mul (SignedInt as) (SignedInt bs) = SignedInt (resize prod) where
    resize xs | prodLen < len = Bin.addLeadingZeros len xs
    resize xs | prodLen > len = Bin.drop (prodLen - len) xs
    resize xs = xs
    prodLen = Bin.length prod
    prod = mulBits (signExtend dlen as) (signExtend dlen bs)
    dlen = 2 * len
    len = Nat.toInt (undefined :: b)

instance ringSignedInt :: Pos b => Ring (SignedInt b) where
  sub (SignedInt as) (SignedInt bs) = SignedInt $ Bin.subtractBits as bs

signedRightShift :: Bits -> Tuple Bit Bits
signedRightShift bits = Bin.rightShift (Bin.msb bits) bits

double :: Bits -> Bits
double = Bin.leftShift _0 >>> \(Tuple o (Bits bits)) -> Bits (A.cons o bits)

half :: Bits -> Bits
half = signedRightShift >>> snd

mulBits :: Bits -> Bits -> Bits
mulBits x _ | Bin.isZero x = _0
mulBits _ y | Bin.isZero y = _0
mulBits x y =
  let z = mulBits x (half y)
  in if Bin.isEven y then double z else Bin.addBits' _0 x (double z)

-- instance baseNSignedInt :: Pos b => BaseN (SignedInt b) where
--   toBase r s | isNegative s = "-" <> toBase r (negate s)
--   toBase r (SignedInt (Bits bits)) = toStringAs r (Bits $ fromMaybe [_0] (A.tail bits))
