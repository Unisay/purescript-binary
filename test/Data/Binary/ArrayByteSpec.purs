module Data.ArrayByte.Spec (spec) where

import Data.Array as A
import Control.Monad.Eff.Random (RANDOM)
import Data.Binary (Bit(Bit), Overflow(Overflow), add, fromBits, fromInt, leftShift, rightShift, toBinString, toBits, tryFromBinStringElastic, tryToInt, zero)
import Data.Binary.Arbitraty (ArbArrayByte(ArbArrayByte), ArbBit(..))
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Data.String (length, toCharArray)
import Test.QuickCheck (Result, (<?>), (===))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)
import Prelude hiding (add,zero)

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

propToStringLength :: ArbArrayByte -> Result
propToStringLength (ArbArrayByte xs) =
  length (toBinString xs) `mod` 8 == 0
    <?> "Length of a string representation of an array of bytes isn't multiple of 8"

propHasBinDigits :: ArbArrayByte -> Result
propHasBinDigits (ArbArrayByte bs) =
  (all (\d -> d == '1' || d == '0') $ toCharArray (toBinString bs))
    <?> "String representation of Byte Array contains not only digits 1 and 0"

propBitsRoundtrip :: ArbArrayByte -> Result
propBitsRoundtrip (ArbArrayByte bs) =
  fromBits (toBits bs) == bs
    <?> "Can't roundtrip byte array: " <> show bs

propStringRoundtrip :: ArbArrayByte -> Result
propStringRoundtrip (ArbArrayByte bs) =
  tryFromBinStringElastic (toBinString bs) === Just bs

propIntRoundtrip :: ArbArrayByte -> Result
propIntRoundtrip (ArbArrayByte bs) =
  case tryToInt bs of
    (Just i) -> fromInt i === bs
    Nothing -> A.length (toBits bs) > 31 <?> "Failed to convert bytes to Int"

propAdditionLeftIdentity :: ArbArrayByte -> Result
propAdditionLeftIdentity (ArbArrayByte a) =
  let (Overflow (Bit b) r) = add zero a
  in not b && r == a <?> "Left identity for " <> show a

propAdditionRightIdentity :: ArbArrayByte -> Result
propAdditionRightIdentity (ArbArrayByte a) =
  let (Overflow (Bit b) r) = add a zero
  in not b && r == a <?> "Right identity for " <> show a

propLeftShift :: ArbBit -> ArbArrayByte -> Boolean
propLeftShift (ArbBit bit) (ArbArrayByte bytes) =
  let (Overflow o shifted) = leftShift bit bytes
      originalBits = toBits bytes
      shiftedBits = toBits shifted
  in Just shiftedBits == A.tail (A.snoc originalBits bit)
  && A.head originalBits == Just o

propRightShift :: ArbBit -> ArbArrayByte -> Boolean
propRightShift (ArbBit bit) (ArbArrayByte bytes) =
  let (Overflow o shifted) = rightShift bit bytes
      originalBits = toBits bytes
      shiftedBits = toBits shifted
  in Just shiftedBits == A.init (A.cons bit originalBits)
  && A.last originalBits == Just o
