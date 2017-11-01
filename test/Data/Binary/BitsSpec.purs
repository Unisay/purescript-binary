module Data.Binary.Bits.Spec (spec) where

import Control.Monad.Eff.Random (RANDOM)
import Data.Array as A
import Data.Binary (Bit(Bit), Bits(Bits), _0, add, addLeadingZeros, diffBits, diffElastic, divMod, double, extendAdd, fromBits, fromInt, half, isNegative, leftShift, length, lsb, msb, multiply, rightShift, stripLeadingZeros, toBinString, toBits, tryFromBinStringElastic, tryFromBits, tryToInt)
import Data.Binary.Arbitrary (ArbBit(ArbBit), ArbBits(ArbBits), ArbBits32(..), ArbInt(..), ArbNonNegativeInt(..), NonOverflowingMultiplicands(..))
import Data.Binary.Overflow (Overflow(..), overflow)
import Data.Foldable (all)
import Data.Maybe (Maybe(..), isJust)
import Data.Ord (abs, lessThan)
import Data.String as Str
import Data.Tuple (Tuple(..))
import Prelude hiding (add)
import Test.QuickCheck (Result(..), (<?>), (===))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = suite "Bits" do
  test "compare" $ quickCheck propCompare
  test "addLeadingZeros" $ quickCheck propAddLeadingZeros
  test "stripLeadingZeros" $ quickCheck propStripLeadingZeros
  test "toBinString contains only 0 and 1" $ quickCheck propHasBinDigits
  test "toBits >>> fromBits" $ quickCheck propBitsRoundtrip
  test "tryToInt >>> fromInt" $ quickCheck propIntRoundtrip
  test "toBinString >>> fromBinString" $ quickCheck propStringRoundtrip
  test "addition left identity" $ quickCheck propAdditionLeftIdentity
  test "addition right identity" $ quickCheck propAdditionRightIdentity
  test "left shift" $ quickCheck propLeftShift
  test "right shift" $ quickCheck propRightShift
  test "least significant bit" $ quickCheck propLsb
  test "most significant bit" $ quickCheck propMsb
  test "diffBits" $ quickCheck propDiffBits
  test "addLeadingZeros" $ quickCheck propAddLeadingZeros
  test "double" $ quickCheck propDouble
  test "half"   $ quickCheck propHalf
  test "diff"   $ quickCheck propDiff
  test "div"    $ quickCheck propDiv
  test "mod"    $ quickCheck propMod
  test "add"    $ quickCheck propAdd
  test "mul"    $ quickCheck propMultiply

propCompare :: ArbInt -> ArbInt -> Result
propCompare (ArbInt a) (ArbInt b) =
  expected == actual
    <?> "\ncompare " <> show a  <> " " <> show b  <> " = " <> show expected
    <>  "\ncompare " <> show as <> " " <> show bs <> " = " <> show actual
  where
    expected = compare a b
    actual = compare as bs
    as :: Bits
    as = fromInt a
    bs = fromInt b

propAddLeadingZeros :: ArbNonNegativeInt -> ArbBits -> Result
propAddLeadingZeros (ArbNonNegativeInt arbitraryLength) (ArbBits bits) =
  expectedLength == actualLength
    <?> "\nExpected length: " <> show expectedLength
    <>  "\nActual length:   " <> show actualLength
    <>  "\nBits:            " <> show bits
  where
    targetLength = arbitraryLength `mod` 1024
    (Bits bs) = addLeadingZeros targetLength bits
    expectedLength = max (length bits) targetLength
    actualLength = A.length bs

propStripLeadingZeros :: ArbBits -> Boolean
propStripLeadingZeros (ArbBits bs) =
   let (Bits bits) = stripLeadingZeros bs
   in A.length bits == 1 || A.head bits /= Just _0

propHasBinDigits :: ArbBits -> Result
propHasBinDigits (ArbBits bs) =
  (all (\d -> d == '1' || d == '0') $ Str.toCharArray (toBinString bs))
    <?> "String representation of Byte Array contains not only digits 1 and 0"

propBitsRoundtrip :: ArbBits -> Result
propBitsRoundtrip (ArbBits bs) =
  fromBits (toBits bs) == bs
    <?> "Can't roundtrip byte array: " <> show bs

propStringRoundtrip :: ArbBits -> Result
propStringRoundtrip (ArbBits bs) =
  Just bs === tryFromBinStringElastic (toBinString bs)

propIntRoundtrip :: ArbBits32 -> Result
propIntRoundtrip (ArbBits32 bits) =
  case tryToInt bits of
    Just i -> bits == fromInt i
      <?> "Failed to read bits as correct Int (" <> show i <> "). Actual bits " <> show bits <> " "
      <> "Bits from int: " <> show ((fromInt i) :: Bits)
    Nothing -> Failed ("Failed to read bits as Int: " <> show bits)

propAdditionLeftIdentity :: ArbBits -> Result
propAdditionLeftIdentity (ArbBits a) =
  let (Overflow (Bit b) r) = add _0 a
  in not b && r == a <?> "Left identity for " <> show a

propAdditionRightIdentity :: ArbBits -> Result
propAdditionRightIdentity (ArbBits a) =
  let (Overflow (Bit b) r) = add a _0
  in not b && r == a <?> "Right identity for " <> show a

propLeftShift :: ArbBit -> ArbBits -> Result
propLeftShift (ArbBit bit) (ArbBits bits) =
  let (Overflow o shifted) = leftShift bit bits
      expected = toBits bits <> toBits bit
      actual = toBits o <> toBits shifted
  in expected === actual

propRightShift :: ArbBit -> ArbBits -> Result
propRightShift (ArbBit bit) (ArbBits bits) =
  let (Overflow o shifted) = rightShift bit bits
      expected = toBits bit <> toBits bits
      actual = toBits shifted <> toBits o
  in expected === actual

propLsb :: ArbBits -> Result
propLsb (ArbBits bits) =
  expected == actual <?> "bits: " <> show bits
                     <> ", lsb = " <> show actual
                     <> ", expected = " <> show expected
  where expected = overflow (rightShift _0 bits)
        actual = lsb bits

propMsb :: ArbBits -> Result
propMsb (ArbBits bits) =
  expected == actual <?> "bits: " <> show bits
                     <> ", msb = " <> show actual
                     <> ", expected = " <> show expected
  where expected = overflow (leftShift _0 bits)
        actual = msb bits

propDiffBits :: ArbBits -> ArbBits -> Boolean
propDiffBits (ArbBits as@(Bits abits)) (ArbBits bs@(Bits bbits)) =
  len == resLen where
    (Bits res) = diffBits as bs
    resLen = A.length res
    len = max (A.length abits) (A.length bbits)

propDouble :: ArbInt -> Result
propDouble (ArbInt i) =
  if i < maxInt
  then Just (i * 2) === tryToInt (double bits)
  else Success
  where maxInt = (top :: Int) / 2
        bits = fromInt i :: Bits

propHalf :: ArbNonNegativeInt -> Result
propHalf (ArbNonNegativeInt i) =
  Just (i / 2) === tryToInt (half bits)
  where bits = fromInt i :: Bits

propDiff :: ArbNonNegativeInt -> ArbNonNegativeInt -> Result
propDiff (ArbNonNegativeInt a) (ArbNonNegativeInt b) =
  Just intRes == binRes
    <?> "diff " <> toBinString as <> " " <> toBinString bs <> " failed: "
                <> show binRes <> " /= " <> show intRes
  where
    intRes = abs (a - b)
    binRes = tryToInt (diffElastic as bs)
    as = fromInt a :: Bits
    bs = fromInt b :: Bits

propDiv :: ArbNonNegativeInt -> ArbNonNegativeInt -> Result
propDiv (ArbNonNegativeInt a) (ArbNonNegativeInt b) =
  Just (a `div` b) === tryToInt dv where
    as = fromInt a :: Bits
    bs = fromInt b :: Bits
    (Tuple dv _) = divMod as bs

propMod :: ArbNonNegativeInt -> ArbNonNegativeInt -> Result
propMod (ArbNonNegativeInt a) (ArbNonNegativeInt b) =
  Just (a `mod` b) === tryToInt md where
    as = fromInt a :: Bits
    bs = fromInt b :: Bits
    (Tuple _ md) = divMod as bs

propAdd :: ArbNonNegativeInt -> ArbNonNegativeInt -> Result
propAdd (ArbNonNegativeInt a) (ArbNonNegativeInt b) =
  Just (a + b) === tryToInt res where
    as = fromInt a :: Bits
    bs = fromInt b :: Bits
    res = extendAdd as bs

propMultiply :: NonOverflowingMultiplicands -> Result
propMultiply (NonOverflowingMultiplicands (Tuple a b)) =
  toBinString ires === toBinString bres where
    as = fromInt a :: Bits
    bs = fromInt b :: Bits
    ires :: Bits
    ires = fromInt (a * b)
    bres = multiply as bs
