module Data.Binary.Bins.Spec (spec) where

import Data.Array as A
import Control.Monad.Eff.Random (RANDOM)
import Data.Binary (Bit(Bit), Overflow(Overflow), add, fromBits, fromInt, leftShift, rightShift, toBinString, toBits, tryFromBinStringElastic, tryToInt)
import Data.Binary.Arbitraty (ArbBinsByte(ArbBinsByte), ArbBit(ArbBit))
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Data.String (length, toCharArray)
import Test.QuickCheck (Result, (<?>), (===))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)
import Prelude hiding (add)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = suite "Array of Bytes" do
  test "toBinString has length multiple of 8" $ quickCheck propToStringLength
  test "toBinString contains only 0 and 1" $ quickCheck propHasBinDigits
  test "toBits >>> fromBits" $ quickCheck propBitsRoundtrip
  test "toInt >>> tryFromInt" $ quickCheck propIntRoundtrip
  test "toBinString >>> fromBinString" $ quickCheck propStringRoundtrip
  test "addition left identity" $ quickCheck propAdditionLeftIdentity
  test "addition right identity" $ quickCheck propAdditionRightIdentity
  test "left shift" $ quickCheck propLeftShift
  test "right shift" $ quickCheck propRightShift

propToStringLength :: ArbBinsByte -> Result
propToStringLength (ArbBinsByte xs) =
  length (toBinString xs) `mod` 8 == 0
    <?> "Length of a string representation of an array of bytes isn't multiple of 8"

propHasBinDigits :: ArbBinsByte -> Result
propHasBinDigits (ArbBinsByte bs) =
  (all (\d -> d == '1' || d == '0') $ toCharArray (toBinString bs))
    <?> "String representation of Byte Array contains not only digits 1 and 0"

propBitsRoundtrip :: ArbBinsByte -> Result
propBitsRoundtrip (ArbBinsByte bs) =
  fromBits (toBits bs) == bs
    <?> "Can't roundtrip byte array: " <> show bs

propStringRoundtrip :: ArbBinsByte -> Result
propStringRoundtrip (ArbBinsByte bs) =
  tryFromBinStringElastic (toBinString bs) === Just bs

propIntRoundtrip :: ArbBinsByte -> Result
propIntRoundtrip (ArbBinsByte bs) =
  case tryToInt bs of
    (Just i) -> fromInt i === bs
    Nothing -> A.length (toBits bs) > 31 <?> "Failed to convert bytes to Int"

propAdditionLeftIdentity :: ArbBinsByte -> Result
propAdditionLeftIdentity (ArbBinsByte a) =
  let (Overflow (Bit b) r) = add zero a
  in not b && r == a <?> "Left identity for " <> show a

propAdditionRightIdentity :: ArbBinsByte -> Result
propAdditionRightIdentity (ArbBinsByte a) =
  let (Overflow (Bit b) r) = add a zero
  in not b && r == a <?> "Right identity for " <> show a

propLeftShift :: ArbBit -> ArbBinsByte -> Boolean
propLeftShift (ArbBit bit) (ArbBinsByte bytes) =
  let (Overflow o shifted) = leftShift bit bytes
      originalBits = toBits bytes
      shiftedBits = toBits shifted
  in Just shiftedBits == A.tail (A.snoc originalBits bit)
  && A.head originalBits == Just o

propRightShift :: ArbBit -> ArbBinsByte -> Boolean
propRightShift (ArbBit bit) (ArbBinsByte bytes) =
  let (Overflow o shifted) = rightShift bit bytes
      originalBits = toBits bytes
      shiftedBits = toBits shifted
  in Just shiftedBits == A.init (A.cons bit originalBits)
  && A.last originalBits == Just o
