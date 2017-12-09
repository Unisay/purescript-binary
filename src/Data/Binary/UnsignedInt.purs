module Data.Binary.UnsignedInt
  ( UnsignedInt
  , magnitude
  , fromInt
  , toInt
  , asBits
  , tryAsBits
  , divModUnsigned
  ) where

import Prelude

import Control.Plus (empty)
import Data.Array (concat, (:))
import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Binary (class Binary, Bit(..), Bits(Bits), Overflow(NoOverflow), _0, _1, tryFromBits)
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
import Unsafe.Coerce (unsafeCoerce)


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

magnitude :: ∀ b. UnsignedInt b -> b
magnitude _ = undefined

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


bitsToStringAs :: Int -> (Array Bit -> Char) -> Bits -> String
bitsToStringAs nb dict (Bits bts) = Str.fromCharArray (rec nb dict bts empty)
  where
  rec :: Int -> (Array Bit -> Char) -> Array Bit -> Array Char -> Array Char
  rec _ _ [] acc = acc
  rec n f bits acc = rec n f (A.dropEnd n bits) (f nBits : acc) where
    nBits = let bs = A.takeEnd n bits
                d = n - A.length bs
            in if d > 0 then A.replicate d _0 <> bs else bs

stringToBits :: ∀ b . Pos b => (Char -> Maybe (Array Bit)) -> String -> Maybe (UnsignedInt b)
stringToBits f = tryToBits >=> tryFromBits where
  tryToBits = Str.toCharArray >>> traverse f >>> map (concat >>> Bits)

instance baseNUnsignedInt :: Pos b => BaseN (UnsignedInt b) where
  toStringAs Bin (UnsignedInt bits) = Bin.toString bits

  toStringAs Oct (UnsignedInt bits) = bitsToStringAs 3 f bits
    where
    f [(Bit false),(Bit false),(Bit false)] = '0'
    f [(Bit false),(Bit false),(Bit true )] = '1'
    f [(Bit false),(Bit true ),(Bit false)] = '2'
    f [(Bit false),(Bit true ),(Bit true )] = '3'
    f [(Bit true ),(Bit false),(Bit false)] = '4'
    f [(Bit true ),(Bit false),(Bit true )] = '5'
    f [(Bit true ),(Bit true ),(Bit false)] = '6'
    f [(Bit true ),(Bit true ),(Bit true )] = '7'
    f bs = unsafeCoerce bs

  toStringAs Hex (UnsignedInt bits) = bitsToStringAs 4 f bits
    where
    f [(Bit false),(Bit false),(Bit false),(Bit false)] = '0'
    f [(Bit false),(Bit false),(Bit false),(Bit true )] = '1'
    f [(Bit false),(Bit false),(Bit true ),(Bit false)] = '2'
    f [(Bit false),(Bit false),(Bit true ),(Bit true )] = '3'
    f [(Bit false),(Bit true ),(Bit false),(Bit false)] = '4'
    f [(Bit false),(Bit true ),(Bit false),(Bit true )] = '5'
    f [(Bit false),(Bit true ),(Bit true ),(Bit false)] = '6'
    f [(Bit false),(Bit true ),(Bit true ),(Bit true )] = '7'
    f [(Bit true ),(Bit false),(Bit false),(Bit false)] = '8'
    f [(Bit true ),(Bit false),(Bit false),(Bit true )] = '9'
    f [(Bit true ),(Bit false),(Bit true ),(Bit false)] = 'a'
    f [(Bit true ),(Bit false),(Bit true ),(Bit true )] = 'b'
    f [(Bit true ),(Bit true ),(Bit false),(Bit false)] = 'c'
    f [(Bit true ),(Bit true ),(Bit false),(Bit true )] = 'd'
    f [(Bit true ),(Bit true ),(Bit true ),(Bit false)] = 'e'
    f [(Bit true ),(Bit true ),(Bit true ),(Bit true )] = 'f'
    f bs = unsafeCoerce bs

  toStringAs r (UnsignedInt bs) = Str.fromCharArray (req bs []) where -- TODO better decimal converion using doubling method
    req bits acc | (UnsignedInt bits :: UnsignedInt b) < UnsignedInt (Base.toBits r) =
      unsafeAsChars bits <> acc
    req bits acc =
      let (Tuple quo rem) = bits `divModUnsigned` (Base.toBits r)
      in req quo (unsafeAsChars rem <> acc)
    unsafeAsChars bb = A.singleton $ unsafePartial $ fromJust $ chars !! Bin.unsafeBitsToInt bb
    chars = Map.keys (Base.alphabet r)

  fromStringAs _ "" = Nothing

  fromStringAs Bin s = Bin.fromString s >>= tryFromBits

  fromStringAs Oct s = stringToBits f s where
    f '0' = Just [_0,_0,_0]
    f '1' = Just [_0,_0,_1]
    f '2' = Just [_0,_1,_0]
    f '3' = Just [_0,_1,_1]
    f '4' = Just [_1,_0,_0]
    f '5' = Just [_1,_0,_1]
    f '6' = Just [_1,_1,_0]
    f '7' = Just [_1,_1,_1]
    f _   = Nothing

  fromStringAs Hex s = stringToBits f s where
    f '0' = Just [_0,_0,_0,_0]
    f '1' = Just [_0,_0,_0,_1]
    f '2' = Just [_0,_0,_1,_0]
    f '3' = Just [_0,_0,_1,_1]
    f '4' = Just [_0,_1,_0,_0]
    f '5' = Just [_0,_1,_0,_1]
    f '6' = Just [_0,_1,_1,_0]
    f '7' = Just [_0,_1,_1,_1]
    f '8' = Just [_1,_0,_0,_0]
    f '9' = Just [_1,_0,_0,_1]
    f 'a' = Just [_1,_0,_1,_0]
    f 'b' = Just [_1,_0,_1,_1]
    f 'c' = Just [_1,_1,_0,_0]
    f 'd' = Just [_1,_1,_0,_1]
    f 'e' = Just [_1,_1,_1,_0]
    f 'f' = Just [_1,_1,_1,_1]
    f _   = Nothing

  fromStringAs radix str = fst <$> A.foldr f (Tuple zero one) <$> traverse t cs where
    f i (Tuple r p) = Tuple (p * i + r) (p * base)
    t = charToBits radix >=> Bin.tryFromBits
    cs = Str.toCharArray (Str.toLower str)
    charToBits r = flip Map.lookup (Base.alphabet radix)
    base = UnsignedInt (Base.toBits radix)
