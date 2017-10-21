module Data.Binary.Bits.Spec (spec) where

import Prelude hiding (add)

import Control.Monad.Eff.Random (RANDOM)
import Data.Array as A
import Data.Binary (_0, add, leftShift, rightShift, toBinString, toBits, tryToInt)
import Data.Binary.Arbitrary (ArbBit(ArbBit), ArbBits(ArbBits), ArbNonNegativeInt(..))
import Data.Binary.Bit (Bit(..))
import Data.Binary.Bits (Bits(..))
import Data.Binary.Elastic (addLeadingZeros, fromBits, fromInt, stripLeadingZeros, tryFromBinStringElastic)
import Data.Binary.Overflow (Overflow(..))
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Data.String as Str
import Test.QuickCheck (Result, (<?>), (===))
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

propCompare :: ArbNonNegativeInt -> ArbNonNegativeInt -> Result
propCompare (ArbNonNegativeInt a) (ArbNonNegativeInt b) =
  compare a b === compare as bs where
    as :: Bits
    as = fromInt a
    bs = fromInt b

propAddLeadingZeros :: ArbNonNegativeInt -> ArbBits -> Result
propAddLeadingZeros (ArbNonNegativeInt expectedLength) (ArbBits bits) =
  expectedLength === actualLength
  where
  (Bits bs) = addLeadingZeros expectedLength bits
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
  tryFromBinStringElastic (toBinString bs) === Just bs

propIntRoundtrip :: ArbBits -> Result
propIntRoundtrip (ArbBits bs@(Bits bits)) =
  case tryToInt bs of
    (Just i) -> stripLeadingZeros bs === fromInt i
    Nothing -> A.length bits > 31 <?> "Failed to convert bytes to Int"

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
