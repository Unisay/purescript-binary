module Data.Binary.Byte.Spec (spec) where

import Control.Monad.Eff.Random (RANDOM)
import Data.Binary.Arbitrary (ArbBit(..), ArbByte(..))
import Data.Binary.Byte (Byte(..))
import Data.Binary.Class (Bit(..), add, leftShift, modAdd, modMul, rightShift, toBinString, toBits, toInt, tryFromBinString, tryFromBits, tryFromInt)
import Data.Binary.Nibble (Nibble(..))
import Data.Binary.Overflow (Overflow(..))
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Data.String (length, toCharArray)
import Prelude hiding (add)
import Test.QuickCheck (Result, (<?>), (===))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = suite "Byte" do
  test "toBinString has length <= 8" $ quickCheck propToStringLength
  test "toBinString contains only 0 and 1" $ quickCheck propHasBinDigits
  test "toBits >>> tryFromBits" $ quickCheck propBitsRoundtrip
  test "toInt >>> tryFromInt" $ quickCheck propIntRoundtrip
  test "toBinString >>> fromBinString" $ quickCheck propStringRoundtrip
  test "addition works like Int" $ quickCheck propAddition
  test "left shift" $ quickCheck propLeftShift
  test "right shift" $ quickCheck propRightShift
  test "modular addition" $ quickCheck propModAdd
  test "modular multiplication" $ quickCheck propModMul

propToStringLength :: ArbByte -> Result
propToStringLength (ArbByte n) = 8 >= length (toBinString n)
  <?> "length (toBinString Byte) > 8"

propHasBinDigits :: ArbByte -> Result
propHasBinDigits (ArbByte n) = (all (\d -> d == '1' || d == '0') $ toCharArray (toBinString n))
  <?> "String representation of Byte contains not only digits 1 and 0"

propStringRoundtrip :: ArbByte -> Result
propStringRoundtrip (ArbByte n) = tryFromBinString (toBinString n) === Just n

propBitsRoundtrip :: ArbByte -> Result
propBitsRoundtrip (ArbByte n) = tryFromBits (toBits n) === Just n

propIntRoundtrip :: ArbByte -> Result
propIntRoundtrip (ArbByte n) = tryFromInt (toInt n) === Just n

propAddition :: ArbByte -> ArbByte -> Result
propAddition (ArbByte a) (ArbByte b) =
  case add a b of
    (Overflow (Bit true) _) ->
      toInt a + toInt b > 15 <?> ("Unexpected overflow bit (" <> show a <> "" <> show b <> ")")
    (Overflow (Bit false) r) ->
      let res = toInt a + toInt b == toInt r
      in res <?> "(toInt a + toInt b) /= toInt (add a b)"
              <> ", where a = " <> show a <> " (" <> show (toInt a) <> ")"
              <> ", b = " <> show b <> " (" <> show (toInt b) <> ")"
              <> ", add a b = " <> show r <> " (" <> show (toInt r) <> ")"


propLeftShift :: ArbByte -> ArbBit -> Result
propLeftShift (ArbByte b@(Byte (Nibble a0 a1 a2 a3) (Nibble a4 a5 a6 a7))) (ArbBit a8)  =
  let (Overflow b0 (Byte (Nibble b1 b2 b3 b4) (Nibble b5 b6 b7 b8))) = leftShift a8 b
  in [a0, a1, a2, a3, a4, a5, a6, a7, a8] === [b0, b1, b2, b3, b4, b5, b6, b7, b8]

propRightShift :: ArbBit -> ArbByte -> Result
propRightShift (ArbBit a0) (ArbByte b@(Byte (Nibble a1 a2 a3 a4) (Nibble a5 a6 a7 a8))) =
  let (Overflow b8 (Byte (Nibble b0 b1 b2 b3) (Nibble b4 b5 b6 b7))) = rightShift a0 b
  in [a0, a1, a2, a3, a4, a5, a6, a7, a8] === [b0, b1, b2, b3, b4, b5, b6, b7, b8]

propModAdd :: ArbByte -> ArbByte -> Result
propModAdd (ArbByte a) (ArbByte b) =
  (ia + ib) `mod` 256 === toInt (modAdd a b) where
    ia = toInt a
    ib = toInt b

propModMul :: ArbByte -> ArbByte -> Result
propModMul (ArbByte a) (ArbByte b) =
  (ia * ib) `mod` 256 === toInt (modMul a b) where
    ia = toInt a
    ib = toInt b
