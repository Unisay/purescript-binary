module Data.Binary.Bits.Spec (spec) where

import Control.Monad.Eff.Random (RANDOM)
import Data.Binary (Overflow(Overflow), _0, add, fromBits, fromInt, leftShift, rightShift, toBinString, toBits, tryFromBinStringElastic, tryToInt)
import Data.Binary.Arbitrary (ArbBit(ArbBit), ArbBits(ArbBits), ArbNonNegativeInt(..))
import Data.Bit (Bit(..))
import Data.Bits (bitsLength, firstBit, stripLeadingZeros, zeroWiden)
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Data.String as Str
import Prelude hiding (add)
import Test.QuickCheck (Result, (<?>), (===))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = suite "Bits" do
  test "zeroWiden" $ quickCheck propZeroWiden
  test "stripLeadingZeros" $ quickCheck propStripLeadingZeros
  test "toBinString contains only 0 and 1" $ quickCheck propHasBinDigits
  test "toBits >>> fromBits" $ quickCheck propBitsRoundtrip
  test "tryToInt >>> fromInt" $ quickCheck propIntRoundtrip
  test "toBinString >>> fromBinString" $ quickCheck propStringRoundtrip
  test "addition left identity" $ quickCheck propAdditionLeftIdentity
  test "addition right identity" $ quickCheck propAdditionRightIdentity
  test "left shift" $ quickCheck propLeftShift
  test "right shift" $ quickCheck propRightShift

propZeroWiden :: ArbNonNegativeInt -> ArbBits -> Result
propZeroWiden (ArbNonNegativeInt i) (ArbBits bits) = i === bitsLength (zeroWiden i bits)

propStripLeadingZeros :: ArbBits -> Boolean
propStripLeadingZeros (ArbBits bs) =
   let bits = stripLeadingZeros bs
   in bitsLength bits == 1 || firstBit bits /= _0

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
  tryFromBinStringElastic (toBinString bs) === Just bs

propIntRoundtrip :: ArbBits -> Result
propIntRoundtrip (ArbBits bs) =
  case tryToInt bs of
    (Just i) -> stripLeadingZeros bs === fromInt i
    Nothing -> bitsLength bs > 31 <?> "Failed to convert bytes to Int"

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
  in toBinString bits <> toBinString bit === toBinString o <> toBinString shifted

propRightShift :: ArbBit -> ArbBits -> Result
propRightShift (ArbBit bit) (ArbBits bits) =
  let (Overflow o shifted) = rightShift bit bits
  in toBinString bit <> toBinString bits === toBinString shifted <> toBinString o
