module Data.Binary.SignedInt
  ( SignedInt
  , fromInt
  , toInt
  , isNegative
  , complement
  , flipSign
  , toNumberAs
  , divMod
  ) where

import Prelude

import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Array ((:))
import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Binary (class Binary, Bit(Bit), Bits(Bits), Overflow(NoOverflow), _0, _1)
import Data.Binary as Bin
import Data.Binary.BaseN (class BaseN, Radix(Radix), toStringAs, unsafeBitsAsChars)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (abs)
import Data.String as Str
import Data.Tuple (Tuple(..), fst, snd, uncurry)
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

unsafeIncrement :: Bits -> Bits
unsafeIncrement = Bin.discardOverflow <<< Bin.addBits _1 Bin.zero

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

signShrink :: Int -> Bits -> Bits
signShrink width bits@(Bits bs) =
  if len <= width
    then bits
    else if h1 == h2
           then signShrink (width - 1) t
           else bits
  where
    h1 = Bin.head bits
    h2 = Bin.head t
    t = Bin.tail bits
    len = Bin.length bits

signAlign :: Bits -> Bits -> Tuple Bits Bits
signAlign bas@(Bits as) bbs@(Bits bs) =
  case compare la lb of
  EQ -> Tuple bas bbs
  LT -> Tuple (signExtend lb bas) bbs
  GT -> Tuple bas (signExtend la bbs)
  where la = A.length as
        lb = A.length bs

instance semiringSignedInt :: Pos b => Semiring (SignedInt b) where
  zero = SignedInt Bin.zero
  add (SignedInt as) (SignedInt bs) = Bin.unsafeFromBits result where
    nBits = Nat.toInt (undefined :: b)
    result = wrapBitsOverflow nBits (Bin.addBits _0 as bs)
    wrapBitsOverflow _ (NoOverflow bits) = bits
    wrapBitsOverflow n res =
      let numValues = Bits (_1 : A.replicate n _0)
      in Bin.tail $ subtractBits (Bin.extendOverflow res) numValues
  one = SignedInt Bin.one
  mul m@(SignedInt mBits) (SignedInt rBits) = SignedInt $ res
    where
      res = iter rlen p
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


dbg :: ∀ a . String -> a -> a
dbg s a = let _ = unsafePerformEff (log s) in a

instance ringSignedInt :: Pos b => Ring (SignedInt b) where
  sub (SignedInt as) (SignedInt bs) = SignedInt $ subtractBits as bs

signedRightShift :: Bits -> Tuple Bit Bits
signedRightShift bits = Bin.rightShift (Bin.msb bits) bits

double :: Bits -> Bits
double = Bin.leftShift _0 >>> \(Tuple o (Bits bits)) -> Bits (A.cons o bits)

increment :: Bits -> Bits
increment a = Bin.discardOverflow $ Bin.addBits _1 Bin.zero a

half :: Bits -> Bits
-- | https://en.wikipedia.org/wiki/Arithmetic_shift#Non-equivalence_of_arithmetic_right_shift_and_division
half a | isNegative a && Bin.isOdd a = half (increment a)
half a = snd (signedRightShift a)

subtractBits :: Bits -> Bits -> Bits
subtractBits as bs = uncurry Bin.subtractBits $ signAlign as bs

divMod :: Bits -> Bits -> Tuple Bits Bits
divMod x _ | Bin.isZero x = Tuple Bin.zero Bin.zero
divMod x y =
  let t = divMod (half x) y
      (Tuple q r) = bimap double double t
      r' = if Bin.isOdd x then increment r else r
  in if (SignedInt r' :: SignedInt D64) >= SignedInt y
     then Tuple (increment q) (r' `subtractBits` y)
     else Tuple q r'

div :: Bits -> Bits -> Bits
div a b = fst (divMod a b)

mod :: Bits -> Bits -> Bits
mod a b = snd (divMod a b)

instance baseNSignedInt :: Pos a => BaseN (SignedInt a) where
  toStringAs (Radix r) si@(SignedInt bits) | r == (Bits [_1, _0]) =
    Bin.toString (compact bits) where
      compact b | Bin.msb b == _1 = Bin.one <> Bin.stripLeadingBit _1 b
      compact b = Bin.stripLeadingZeros b
  toStringAs (Radix r) (SignedInt bs) = Str.fromCharArray (req bs []) where
    req bits acc | (SignedInt bits :: SignedInt a) < SignedInt r =
      unsafeBitsAsChars bits <> acc
    req bits acc =
      let (Tuple quo rem) = bits `divMod` r
      in req quo (unsafeBitsAsChars rem <> acc)

-- | Like `toStringAs` but outputs `-` prefix instead of the two's complement
toNumberAs :: ∀ a . Pos a => Radix -> SignedInt a -> String
toNumberAs r si = if isNegative si
                  then "-" <> toStringAs r (complement si)
                  else toStringAs r si

  -- TODO: fromStringAs
