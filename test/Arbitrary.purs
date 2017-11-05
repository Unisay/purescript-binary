module Test.Arbitrary where

import Prelude

import Data.Array as A
import Data.Binary (Bit(..), Bits(..), _0, _1)
import Data.Binary.SignedInt as SI
import Data.Binary.SignedInt (SignedInt)
import Data.Binary.UnsignedInt as UI
import Data.Binary.UnsignedInt (UnsignedInt)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D31, D32, d31, d32)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, frequency, sized, suchThat, vectorOf)

newtype ArbInt4 = ArbInt4 Int
instance arbitraryInt4 :: Arbitrary ArbInt4 where
  arbitrary = ArbInt4 <$> chooseInt (-128) 127

newtype ArbInt = ArbInt Int
derive newtype instance eqArbInt :: Eq ArbInt
instance arbitraryInt :: Arbitrary ArbInt where
  arbitrary = ArbInt <$> frequency gens where
    gens = Tuple 0.05 (pure 0)      :|
           Tuple 0.05 (pure 1)      :
           Tuple 0.05 (pure (-1))   :
           Tuple 0.05 (pure top)    :
           Tuple 0.05 (pure bottom) :
           Tuple 0.75 arbitrary     :
           Nil

newtype ArbNonNegativeInt = ArbNonNegativeInt Int
instance arbitraryNonNegativeInt :: Arbitrary ArbNonNegativeInt where
  arbitrary = ArbNonNegativeInt <$> frequency gens where
    gens = Tuple 0.05 (pure top)
        :| Tuple 0.05 (pure one)
         : Tuple 0.90 (suchThat arbitrary (_ >= 0))
         : Nil

newtype ArbUnsignedInt31 = ArbUnsignedInt31 (UnsignedInt D31)
derive instance newtypeArbUnsignedInt31 :: Newtype ArbUnsignedInt31 _
instance arbitraryUnsignedInt31 :: Arbitrary ArbUnsignedInt31 where
  arbitrary = ArbUnsignedInt31 <$> do
    (ArbNonNegativeInt a) <- arbitrary
    pure (UI.fromInt d31 a)

newtype ArbSignedInt32 = ArbSignedInt32 (SignedInt D32)
derive instance newtypeArbSignedInt32 :: Newtype ArbSignedInt32 _
instance arbitrarySignedInt32 :: Arbitrary ArbSignedInt32 where
  arbitrary = ArbSignedInt32 <$> SI.fromInt d32 <$> arbitrary

newtype NonOverflowingMultiplicands = NonOverflowingMultiplicands (Tuple Int Int)
instance arbitraryNonOverflowingMultiplicands :: Arbitrary NonOverflowingMultiplicands where
  arbitrary = NonOverflowingMultiplicands <$> (flip suchThat nonOverflowing) do
    (ArbNonNegativeInt a) <- arbitrary
    (ArbNonNegativeInt b) <- arbitrary
    pure (Tuple a b)
    where nonOverflowing (Tuple a b) = (toNumber a) * (toNumber b) <= toNumber (top :: Int)

newtype ArbBit = ArbBit Bit
derive instance newtypeArbBit :: Newtype ArbBit _
derive newtype instance eqArbBit :: Eq ArbBit
derive newtype instance showArbBit :: Show ArbBit
instance arbitraryBit :: Arbitrary ArbBit where
  arbitrary = ArbBit <<< Bit <$> arbitrary

newtype ArbBits = ArbBits Bits
derive newtype instance eqArbBits :: Eq ArbBits
derive newtype instance showArbBits :: Show ArbBits
instance arbitraryBits :: Arbitrary ArbBits where
  arbitrary =
    ArbBits <$> Bits <$> arbBits where
      arbBits = sized \s -> vectorOf s arbBit
      arbBit = unwrap <$> (arbitrary :: Gen ArbBit)

newtype ArbBits32 = ArbBits32 Bits
instance arbitraryBits32 :: Arbitrary ArbBits32 where
  arbitrary = ArbBits32 <$> Bits <$> frequency gens where
    gens = Tuple 0.05 (vectorOf 32 (pure _0))
        :| Tuple 0.05 (vectorOf 32 (pure _1))
         : Tuple 0.05 (flip A.snoc _1 <$> vectorOf 31 (pure _0))
         : Tuple 0.05 (flip A.snoc _0 <$> vectorOf 31 (pure _1))
         : Tuple 0.05 (A.cons _1 <$> vectorOf 31 (pure _0))
         : Tuple 0.05 (A.cons _0 <$> vectorOf 31 (pure _1))
         : Tuple 0.70 (vectorOf 32 arbBit)
         : Nil
    arbBit = unwrap <$> (arbitrary :: Gen ArbBit)
