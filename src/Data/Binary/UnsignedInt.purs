module Data.Binary.UnsignedInt
  ( UnsignedInt
  , fromInt
  , toInt
  ) where

import Prelude

import Data.Array ((!!), (:))
import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Binary (class Binary, Bit(Bit), Bits(Bits), Overflow(Overflow, NoOverflow), Radix(Radix), _0, _1, align, and, discardOverflow, extendOverflow, isEven, isOdd, leftShift, length, lsb, makeOverflow, msb, or, overflowBit, rightShift, stripLeadingZeros, tail, unsafeFromBits, xor)
import Data.Binary as Bin
import Data.Binary.BaseN (class BaseN)
import Data.Int as Int
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (unwrap)
import Data.String as Str
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Tuple.Nested (get1)
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

instance eqUnsignedInt :: Pos b => Eq (UnsignedInt b) where
  eq (UnsignedInt bits) (UnsignedInt bits') = eq bits bits'

instance ordUnsignedInt :: Pos b => Ord (UnsignedInt b) where
  compare (UnsignedInt as) (UnsignedInt bs) = (uncurry compare) $ bimap unwrap unwrap $ align as bs

instance showUnsignedInt :: Pos b => Show (UnsignedInt b) where
  show (UnsignedInt bits) =
    "UnsignedInt" <> show (Nat.toInt (undefined :: b)) <> "#" <> Bin.toBinString bits

-- | Converts `Int` value to `UnsignedInt b` for b >= 31
-- | Behavior for negative `Int` values is unspecified.
fromInt :: ∀ b . Pos b => GtEq b D31 => b -> Int -> UnsignedInt b
fromInt _ i | i < zero = _0
fromInt _ i = UnsignedInt $ stripLeadingZeros $ Bits $ intBits i where
    intBits 0 = [_0]
    intBits n | Int.odd n = A.snoc (intBits (n `div` 2)) _1
              | otherwise = A.snoc (intBits (n `div` 2)) _0

toInt :: ∀ b . Pos b => Lt b D32 => UnsignedInt b -> Int
toInt ui@(UnsignedInt bits) = unsafeBitsToInt bits

unsafeBitsToInt :: Bits -> Int
unsafeBitsToInt (Bits bits) = get1 $ A.foldr f (Tuple 0 1) bits where
  f b (Tuple r p) = Tuple (p * Bin.bitToInt b + r) (p * 2)

instance binaryUnsignedInt :: Pos b => Binary (UnsignedInt b) where
  _0 = UnsignedInt _0
  _1 = UnsignedInt _1
  msb (UnsignedInt bits) = msb bits
  lsb (UnsignedInt bits) = lsb bits
  and (UnsignedInt as) (UnsignedInt bs) = UnsignedInt (and as bs)
  xor (UnsignedInt as) (UnsignedInt bs) = UnsignedInt (xor as bs)
  or  (UnsignedInt as) (UnsignedInt bs) = UnsignedInt (or as bs)
  not (UnsignedInt bs) = UnsignedInt (Bin.not bs)
  leftShift bit (UnsignedInt bs) = UnsignedInt <$> Bin.leftShift bit bs
  rightShift bit (UnsignedInt bs) = UnsignedInt <$> Bin.rightShift bit bs
  signedRightShift i = rightShift (msb i) i
  toBits (UnsignedInt bs) = Bin.addLeadingZeros (Nat.toInt (undefined :: b)) bs
  tryFromBits bits = f (stripLeadingZeros bits) where
    f bs | length bs > Nat.toInt (undefined :: b) = Nothing
    f bs = Just $ UnsignedInt bs

instance boundedUnsignedInt :: Pos b => Bounded (UnsignedInt b) where
  bottom = _0
  top = UnsignedInt (Bits (A.replicate (Nat.toInt (undefined :: b)) _1))

instance semiringUnsignedInt :: Pos b => Semiring (UnsignedInt b) where
  zero = _0
  add (UnsignedInt as) (UnsignedInt bs) = unsafeFromBits result where
    nBits = Nat.toInt (undefined :: b)
    result = wrapBitsOverflow nBits (addBits _0 as bs)
    wrapBitsOverflow _ (NoOverflow bits) = bits
    wrapBitsOverflow n res =
      let numValues = _1 <> Bits (A.replicate n _0)
      in Bin.tail $ subtractBits (extendOverflow res) numValues
  one = _1
  mul (UnsignedInt as) (UnsignedInt bs) = unsafeFromBits result where
    nBits = Nat.toInt (undefined :: b)
    rawResult = mulBits as bs
    numValues = Bits $ _1 : (A.replicate nBits _0)
    (Tuple quo rem) = divMod rawResult numValues
    result = if isOdd quo then tail rem else rem

addBit :: Bit -> Bit -> Bit -> Overflow Bit
addBit (Bit false) (Bit false) (Bit false) = NoOverflow _0
addBit (Bit false) (Bit false) (Bit true)  = NoOverflow _1
addBit (Bit false) (Bit true) (Bit false)  = NoOverflow _1
addBit (Bit false) (Bit true) (Bit true)   = Overflow   _0
addBit (Bit true) (Bit false) (Bit false)  = NoOverflow _1
addBit (Bit true) (Bit false) (Bit true)   = Overflow   _0
addBit (Bit true) (Bit true) (Bit false)   = Overflow   _0
addBit (Bit true) (Bit true) (Bit true)    = Overflow   _1

addBits :: Bit -> Bits -> Bits -> Overflow Bits
addBits bit abits@(Bits as) bbits@(Bits bs) =
  Bits <$> A.foldr f acc pairs where
    f :: Tuple Bit Bit -> Overflow (Array Bit) -> Overflow (Array Bit)
    f (Tuple a b) o = flip A.cons (discardOverflow o) <$> addBit (overflowBit o) a b
    acc = makeOverflow bit []
    pairs = uncurry A.zip $ bimap unwrap unwrap $ Bin.align abits bbits

addBits' :: Bit -> Bits -> Bits -> Bits
addBits' bit a b = extendOverflow (addBits bit a b)

isZero :: Bits -> Boolean
isZero = unwrap >>> A.dropWhile (eq _0) >>> A.null

half :: Bits -> Bits
half = rightShift _0 >>> snd

double :: Bits -> Bits
double = leftShift _0 >>> \(Tuple o (Bits bits)) -> Bits (A.cons o bits)

mulBits :: Bits -> Bits -> Bits
mulBits x _ | isZero x = _0
mulBits _ y | isZero y = _0
mulBits x y =
  let z = mulBits x (half y)
  in if isEven y then double z
                 else addBits' _0 x (double z)

divMod :: Bits -> Bits -> Tuple Bits Bits
divMod x _ | isZero x = Tuple _0 _0
divMod x y =
  let inc a = addBits' _0 a _1
      t = divMod (half x) y
      (Tuple q r) = bimap double double t
      r' = if isOdd x then inc r else r
  in if (UnsignedInt r' :: UnsignedInt D32) >= UnsignedInt y
     then Tuple (inc q) (r' `subtractBits` y)
     else Tuple q r'

instance ringUnsignedInt :: Pos b => Ring (UnsignedInt b) where
  sub (UnsignedInt as) (UnsignedInt bs) = UnsignedInt $ subtractBits as bs

subtractBits :: Bits -> Bits -> Bits
subtractBits as bs = Bits acc where
  f :: (Tuple Bit Bit) -> Tuple Boolean (Array Bit) -> Tuple Boolean (Array Bit)
  -- https://i.stack.imgur.com/5M40R.jpg
  f (Tuple (Bit false) (Bit false)) (Tuple false acc) = Tuple false (A.cons _0 acc)
  f (Tuple (Bit false) (Bit false)) (Tuple true  acc) = Tuple true  (A.cons _1 acc)
  f (Tuple (Bit false) (Bit true) ) (Tuple false acc) = Tuple true  (A.cons _1 acc)
  f (Tuple (Bit false) (Bit true) ) (Tuple true  acc) = Tuple true  (A.cons _0 acc)
  f (Tuple (Bit true)  (Bit false)) (Tuple false acc) = Tuple false (A.cons _1 acc)
  f (Tuple (Bit true)  (Bit false)) (Tuple true  acc) = Tuple false (A.cons _0 acc)
  f (Tuple (Bit true)  (Bit true) ) (Tuple false acc) = Tuple false (A.cons _0 acc)
  f (Tuple (Bit true)  (Bit true) ) (Tuple true  acc) = Tuple true  (A.cons _1 acc)
  pairs = uncurry A.zip $ bimap unwrap unwrap $ Bin.align as bs
  (Tuple _ acc) = A.foldr f (Tuple false []) pairs

instance baseNUnsignedInt :: Pos b => BaseN (UnsignedInt b) where
  toBase r ui = toStringAs r ui

toStringAs :: ∀ a . Pos a => Radix -> UnsignedInt a -> String
toStringAs (Radix r) (UnsignedInt bs) = Str.fromCharArray (req bs []) where
  req bits acc | (UnsignedInt bits :: UnsignedInt a) < UnsignedInt r =
    unsafeBitsAsChars bits <> acc
  req bits acc =
    let (Tuple quo rem) = bits `divMod` r
    in req quo (unsafeBitsAsChars rem <> acc)

-- TODO: fromStringAs

alphabet :: Array Char
alphabet = [ '0' ,'1' ,'2' ,'3' ,'4' ,'5' ,'6' ,'7' ,'8' ,'9' , 'a' ,'b' ,'c' ,'d' ,'e' ,'f' ]

unsafeBitsAsChars :: Bits -> Array Char
unsafeBitsAsChars bits = fromMaybe [] $ A.singleton <$> (alphabet !! unsafeBitsToInt bits)
