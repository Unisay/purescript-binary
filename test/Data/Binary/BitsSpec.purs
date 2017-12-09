module Data.Binary.Bits.Spec (spec) where

import Prelude hiding (add)

import Control.Monad.Eff.Random (RANDOM)
import Data.Array as A
import Data.Binary (Bits(Bits), _0)
import Data.Binary as Bin
import Data.Foldable (all)
import Data.Maybe (Maybe(Just))
import Data.String as Str
import Data.Tuple (Tuple(..), fst)
import Test.Arbitrary (ArbBit(ArbBit), ArbBits(ArbBits), ArbNonNegativeInt(ArbNonNegativeInt))
import Test.QuickCheck (Result, (<?>), (===))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = suite "Bits" do
  test "addLeadingZeros" $ quickCheck propAddLeadingZeros
  test "stripLeadingZeros" $ quickCheck propStripLeadingZeros
  test "toBinString contains only 0 and 1" $ quickCheck propHasBinDigits
  test "toBits >>> fromBits" $ quickCheck propBitsRoundtrip
  test "toString >>> fromString" $ quickCheck propStringRoundtrip
  test "left shift" $ quickCheck propLeftShift
  test "right shift" $ quickCheck propRightShift
  test "least significant bit" $ quickCheck propLsb
  test "most significant bit" $ quickCheck propMsb

propAddLeadingZeros :: ArbNonNegativeInt -> ArbBits -> Result
propAddLeadingZeros (ArbNonNegativeInt arbitraryLength) (ArbBits bits) =
  expectedLength == actualLength
    <?> "\nExpected length: " <> show expectedLength
    <>  "\nActual length:   " <> show actualLength
    <>  "\nBits:            " <> show bits
  where
    targetLength = arbitraryLength `mod` 1024
    (Bits bs) = Bin.addLeadingZeros targetLength bits
    expectedLength = max (Bin.length bits) targetLength
    actualLength = A.length bs

propStripLeadingZeros :: ArbBits -> Boolean
propStripLeadingZeros (ArbBits bs) =
   let (Bits bits) = Bin.stripLeadingZeros bs
   in A.length bits == 1 || A.head bits /= Just _0

propHasBinDigits :: ArbBits -> Result
propHasBinDigits (ArbBits bs) =
  (all (\d -> d == '1' || d == '0') $ Str.toCharArray (Bin.toString bs))
    <?> "String representation of Byte Array contains not only digits 1 and 0"

propBitsRoundtrip :: ArbBits -> Result
propBitsRoundtrip (ArbBits bs) =
  Bin.fromBits (Bin.toBits bs) == bs
    <?> "Can't roundtrip byte array: " <> show bs

propStringRoundtrip :: ArbBits -> Result
propStringRoundtrip (ArbBits bs) =
  Just bs === Bin.tryFromBinStringElastic (Bin.toString bs)

propLeftShift :: ArbBit -> ArbBits -> Result
propLeftShift (ArbBit bit) (ArbBits bits) =
  expected == actual
    <?> "\nExpected: " <> show expected
    <>  "\nActual:   " <> show actual
  where
    (Tuple o shifted) = Bin.leftShift bit bits
    expected = Bin.toBits bits <> Bin.toBits bit
    actual = Bin.toBits o <> Bin.toBits shifted

propRightShift :: ArbBit -> ArbBits -> Result
propRightShift (ArbBit bit) (ArbBits bits) =
  let (Tuple o shifted) = Bin.rightShift bit bits
      expected = Bin.toBits bit <> Bin.toBits bits
      actual = Bin.toBits shifted <> Bin.toBits o
  in expected === actual

propLsb :: ArbBits -> Result
propLsb (ArbBits bits) =
  expected == actual <?> "bits: " <> show bits
                     <> ", lsb = " <> show actual
                     <> ", expected = " <> show expected
  where expected = fst (Bin.rightShift _0 bits)
        actual = Bin.lsb bits

propMsb :: ArbBits -> Result
propMsb (ArbBits bits) =
  expected == actual <?> "bits: " <> show bits
                     <> ", msb = " <> show actual
                     <> ", expected = " <> show expected
  where expected = fst (Bin.leftShift _0 bits)
        actual = Bin.msb bits
