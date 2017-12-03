module Data.Binary.SignedInt
  ( SignedInt
  , fromInt
  , fromUnsigned
  , tryFromUnsigned
  , unsafeToUnsigned
  , toInt
  , isNegative
  , complement
  , flipSign
  , toString2c
  ) where

import Data.Array ((:))
import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Binary (class Binary, Bit(Bit), Bits(Bits), Overflow(NoOverflow), _0, _1)
import Data.Binary as Bin
import Data.Binary.BaseN (class BaseN, Radix(..), fromStringAs, toStringAs)
import Data.Binary.BaseN as Base
import Data.Binary.UnsignedInt (UnsignedInt, divModUnsigned)
import Data.List ((!!))
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just), fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (abs)
import Data.String as Str
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Typelevel.Num (class Gt, class GtEq, class Lt, class LtEq, type (:*), D1, D16, D2, D32, D5, D6, D64, D8)
import Data.Typelevel.Num as Nat
import Data.Typelevel.Num.Sets (class Pos)
import Data.Typelevel.Undefined (undefined)
import Partial.Unsafe (unsafePartial)
import Prelude hiding (div,mod)


type Int8   = SignedInt D8
type Int16  = SignedInt D16
type Int32  = SignedInt D32
type Int64  = SignedInt D64
type Int128 = SignedInt (D1 :* D2 :* D8)
type Int256 = SignedInt (D2 :* D5 :* D6)

newtype SignedInt b = SignedInt Bits

derive instance newtypeSignedInt :: Newtype (SignedInt b) _

instance eqSignedInt :: Eq (SignedInt b) where
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
    "SignedInt" <> show (Nat.toInt (undefined :: b)) <> "#" <> Bin.toString bits

flipSign :: ∀ b. SignedInt b -> SignedInt b
flipSign (SignedInt bits) =
  let { head: h, tail: (Bits t) } = Bin.uncons bits
      bs = Bits $ A.cons (Bin.not h) t
  in SignedInt bs

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
    case compare (A.length bs) w of
    GT -> bits
    EQ -> complementBits bits
    LT -> complementBits (Bin.addLeadingZeros w bits)

fromUnsigned :: ∀ a b . Pos a => Pos b => Lt a b => UnsignedInt a -> SignedInt b
fromUnsigned u = SignedInt (Bin.addLeadingZeros b (Bin.toBits u))
  where b = Nat.toInt (undefined :: b)

tryFromUnsigned :: ∀ b . Pos b => UnsignedInt b -> Maybe (SignedInt b)
tryFromUnsigned = Bin.toBits >>> Bin.stripLeadingZeros >>> Bin.tryFromBits

tryToUnsigned :: ∀ b . Pos b => SignedInt b -> Maybe (UnsignedInt b)
tryToUnsigned s | isNegative s = Nothing
tryToUnsigned s = Bin.tryFromBits $ Bin.stripLeadingZeros $ Bin.toBits s

unsafeToUnsigned :: ∀ b . Pos b => SignedInt b -> UnsignedInt b
unsafeToUnsigned s = unsafePartial
                   $ fromJust
                   $ Bin.tryFromBits
                   $ Bin.stripLeadingZeros
                   $ Bin.toBits s

toInt :: ∀ b . Pos b => LtEq b D32 => SignedInt b -> Int
toInt si@(SignedInt bits) =
  if isNegative si
  then negate let (SignedInt bb) = complement si in Bin.unsafeBitsToInt bb
  else Bin.unsafeBitsToInt $ Bin.tail bits

isNegative :: ∀ a . Binary a => a -> Boolean
isNegative = Bin.msb >>> eq _1

instance binarySignedInt :: Pos b => Binary (SignedInt b) where
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
  bottom = SignedInt $ Bits (A.cons _1 (A.replicate (Nat.toInt (undefined :: b) - 1) _0))
  top    = SignedInt $ Bits (A.cons _0 (A.replicate (Nat.toInt (undefined :: b) - 1) _1))

signExtend :: Int -> Bits -> Bits
signExtend width bits | Bin.head bits == _0 = Bin.addLeadingZeros width bits
signExtend width (Bits bits) =
  let d = sub width (A.length bits)
  in Bits if d < 1 then bits else (A.replicate d _1) <> bits

signSquash :: Int -> Bits -> Bits
signSquash width bits | let l = Bin.length bits in l <= width || l < 3 =
  bits
signSquash width bits @ (Bits bs) =
  if A.index bs 0 == A.index bs 1
  then signSquash width (Bin.tail bits)
  else bits

signAlign :: Bits -> Bits -> Tuple Bits Bits
signAlign bas@(Bits as) bbs@(Bits bs) =
  case compare la lb of
  EQ -> Tuple bas bbs
  LT -> Tuple (signExtend lb bas) bbs
  GT -> Tuple bas (signExtend la bbs)
  where la = A.length as
        lb = A.length bs

adjustWidth :: Int -> Bits -> Bits
adjustWidth width bits =
  case compare (Bin.length bits) width of
  EQ -> bits
  LT -> signExtend width bits
  GT -> signSquash width bits

instance semiringSignedInt :: Pos b => Semiring (SignedInt b) where
  zero = SignedInt $ Bits $ A.replicate b _0 where
    b = Nat.toInt (undefined :: b)
  add (SignedInt as) (SignedInt bs) = Bin.unsafeFromBits result where
    b = Nat.toInt (undefined :: b)
    result = wrapBitsOverflow b (Bin.addBits _0 as bs)
    wrapBitsOverflow _ (NoOverflow bits) = bits
    wrapBitsOverflow n res =
      let bigN = Bits (_1 : A.replicate n _0)
      in Bin.tail $ subtractBits (Bin.extendOverflow res) bigN
  one = SignedInt Bin.one
  mul m@(SignedInt mBits) (SignedInt rBits) = SignedInt sres
    where
      sres = Bin.drop (Bin.length res - b) res
      res = iter rlen p `mod` Bits (_1 : A.replicate b _0)
      b = Nat.toInt (undefined :: b)
      a = signExtend (1 + mlen) mBits <> Bin.zeroes (rlen + 1)
      s = nBits <> Bin.zeroes (rlen + 1)
      nBits = complementBits $ signExtend (1 + mlen) mBits
      p = Bin.zeroes (1 + mlen) <> rBits <> Bin.zero
      add k j = Bin.discardOverflow (Bin.addBits _0 k j)
      shr = signedRightShift >>> snd
      mlen = Bin.length mBits
      rlen = Bin.length rBits
      iter 0 t = Bin.init t
      iter y t = iter (y - 1) t' where
        t' = shr $ f (Bin.last $ Bin.init t) (Bin.last t)
        f (Bit false) (Bit true) = add t a
        f (Bit true) (Bit false) = add t s
        f _ _ = t

instance ringSignedInt :: Pos b => Ring (SignedInt b) where
  sub (SignedInt as) (SignedInt bs) = SignedInt $ subtractBits as bs

signedRightShift :: Bits -> Tuple Bit Bits
signedRightShift bits = Bin.rightShift (Bin.msb bits) bits

double :: Bits -> Bits
double = Bin.leftShift _0 >>> \(Tuple o (Bits bits)) -> Bits (A.cons o bits)

increment :: Bits -> Bits
increment a = Bin.discardOverflow $ Bin.addBits _1 Bin.zero a

unsafeIncrement :: Bits -> Bits
unsafeIncrement = Bin.discardOverflow <<< Bin.addBits _1 Bin.zero

half :: Bits -> Bits
-- | https://en.wikipedia.org/wiki/Arithmetic_shift#Non-equivalence_of_arithmetic_right_shift_and_division
half a | isNegative a && Bin.isOdd a = half (increment a)
half a = snd (signedRightShift a)

subtractBits :: Bits -> Bits -> Bits
subtractBits as bs = uncurry Bin.subtractBits $ signAlign as bs

divMod :: ∀ b . Pos b => SignedInt b -> SignedInt b -> Tuple (SignedInt b) (SignedInt b)
divMod (SignedInt dividend) (SignedInt divisor) =
  bimap f f $ dividend `divMod2c` divisor where
  f = adjustWidth b >>> SignedInt
  b = Nat.toInt (undefined :: b)

divMod2c :: Bits -> Bits -> Tuple Bits Bits
divMod2c x _ | Bin.isZero x = Tuple x x
divMod2c x y =
  let nx = isNegative x
      ny = isNegative y
      (Tuple q r) = divModUnsigned (if nx then complementBits x else x)
                                   (if ny then complementBits y else y)
  in Tuple (if nx /= ny then complementBits q else q)
           (if nx then complementBits r else r)

div :: Bits -> Bits -> Bits
div a b = fst (divMod2c a b)

mod :: Bits -> Bits -> Bits
mod a b = snd (divMod2c a b)

instance baseNSignedInt :: (Pos b, Gt b D2) => BaseN (SignedInt b) where
  fromStringAs radix s | Str.take 1 s == "-" = complement <$> fromStringAs radix (Str.drop 1 s)
  fromStringAs radix s = fromStringAs radix s >>= tryFromUnsigned
  toStringAs r si = if isNegative si then "-" <> s (complement si) else s si
    where s = unsafeToUnsigned >>> toStringAs r

-- | two's complement
toString2c :: ∀ b . Pos b => Gt b D2 => Radix -> SignedInt b -> String
toString2c r s = if r == Bin
  then Bin.toString $ adjustWidth b $ Bin.toBits s
  else Str.fromCharArray (req s [])
  where
  req si acc | si /= bottom && abs si < radix = unsafeAsChars si <> acc
  req si acc = let (Tuple quo rem) = si `divMod` radix
               in req quo (unsafeAsChars rem <> acc)
  unsafeAsChars i = A.singleton $ unsafePartial $ fromJust $ mbChar $ Bin.toBits $ abs i
  mbChar bb = chars !! Bin.unsafeBitsToInt bb
  chars = Map.keys (Base.alphabet r)
  radix = SignedInt (Bin.addLeadingZeros b (Base.toBits r))
  b = Nat.toInt (undefined :: b)
