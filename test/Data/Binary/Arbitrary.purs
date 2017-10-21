module Data.Binary.Arbitrary where

import Prelude

import Data.Binary.Byte (Byte(..))
import Data.Binary.Nibble (Nibble(..))
import Data.Binary.Bit (Bit(..))
import Data.Binary.Bits (Bits(..))
import Data.Newtype (class Newtype, unwrap)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, sized, suchThat, vectorOf)

newtype ArbNonNegativeInt = ArbNonNegativeInt Int

instance arbitraryNonNegativeInt :: Arbitrary ArbNonNegativeInt where
  arbitrary = ArbNonNegativeInt <$> suchThat arbitrary (_ >= 0)

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


newtype ArbNibble = ArbNibble Nibble
derive instance newtypeArbNibble :: Newtype ArbNibble _
derive newtype instance eqArbNibble :: Eq ArbNibble
derive newtype instance showArbNibble :: Show ArbNibble

instance arbitraryNibble :: Arbitrary ArbNibble where
  arbitrary = do
   (ArbBit a) <- arbitrary
   (ArbBit b) <- arbitrary
   (ArbBit c) <- arbitrary
   (ArbBit d) <- arbitrary
   pure $ ArbNibble $ Nibble a b c d

newtype ArbByte = ArbByte Byte
derive instance newtypeArbByte :: Newtype ArbByte _
derive newtype instance eqArbByte :: Eq ArbByte
derive newtype instance showArbByte :: Show ArbByte

instance arbitraryByte :: Arbitrary ArbByte where
  arbitrary = do
    (ArbNibble n1) <- arbitrary
    (ArbNibble n2) <- arbitrary
    pure $ ArbByte $ Byte n1 n2
