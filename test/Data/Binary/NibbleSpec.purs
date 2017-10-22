module Data.Binary.Nibble.Spec
  ( spec
  ) where

import Control.Monad.Eff.Random (RANDOM)
import Data.Binary.Arbitrary (ArbBit(ArbBit), ArbNibble(ArbNibble))
import Data.Binary.Class (Bit(..), add, leftShift, rightShift, toBinString, toBits, toInt, tryFromBinString, tryFromBits, tryFromInt)
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
spec = suite "Nibble" do
  test "toBinString has length 4" $ quickCheck propToStringLength
  test "toBinString contains only 0 and 1" $ quickCheck propHasBinDigits
  test "toBits >>> tryFromBits" $ quickCheck propBitsRoundtrip
  test "toInt >>> tryFromInt" $ quickCheck propIntRoundtrip
  test "toBinString >>> fromBinString" $ quickCheck propStringRoundtrip
  test "addition works like Int" $ quickCheck propAddition
  test "left shift" $ quickCheck propLeftShift
  test "right shift" $ quickCheck propRightShift

propToStringLength :: ArbNibble -> Result
propToStringLength (ArbNibble n) = 4 === length (toBinString n)

propHasBinDigits :: ArbNibble -> Result
propHasBinDigits (ArbNibble n) = (all (\d -> d == '1' || d == '0') $ toCharArray (toBinString n))
  <?> "String representation of Nibble contains not only digits 1 and 0"

propBitsRoundtrip :: ArbNibble -> Result
propBitsRoundtrip (ArbNibble n) = tryFromBits (toBits n) === Just n

propStringRoundtrip :: ArbNibble -> Result
propStringRoundtrip (ArbNibble n) = tryFromBinString (toBinString n) === Just n

propIntRoundtrip :: ArbNibble -> Result
propIntRoundtrip (ArbNibble n) = tryFromInt (toInt n) === Just n

propAddition :: ArbNibble -> ArbNibble -> Result
propAddition (ArbNibble a) (ArbNibble b) =
  case add a b of
    (Overflow (Bit true) _) ->
      toInt a + toInt b > 15 <?> "Unexpected overflow bit"
    (Overflow (Bit false) r) ->
      toInt a + toInt b === toInt r

propLeftShift :: ArbNibble -> ArbBit -> Result
propLeftShift (ArbNibble n@(Nibble a b c d)) (ArbBit e) =
  let (Overflow a' (Nibble b' c' d' e')) = leftShift e n
  in [a, b, c, d, e] === [a', b', c', d', e']

propRightShift :: ArbBit -> ArbNibble -> Result
propRightShift (ArbBit a) (ArbNibble n@(Nibble b c d e)) =
  let (Overflow e' (Nibble a' b' c' d')) = rightShift a n
  in [a, b, c, d, e] === [a', b', c', d', e']
