module Data.Nibble.Spec
  ( spec
  ) where

import Control.Monad.Eff.Random (RANDOM)
import Data.Binary (Overflow(..), Bit(..), Nibble(..), add, fromBinString, toBinString, leftShift, rightShift, toInt)
import Data.Binary.Arbitraty (ArbBit(..), ArbNibble(..))
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Data.String (length, toCharArray)
import Test.QuickCheck (Result, (<?>), (===))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)
import Data.Semiring as S
import Prelude hiding (add)


spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = suite "Nibble" do
  test "toBinString has length 4" $ quickCheck propToStringLength
  test "toBinString contains only 0 and 1" $ quickCheck propHasBinDigits
  test "toBinString >>> fromBinString" $ quickCheck propStringRoundtrip
  test "addition works like Int" $ quickCheck propAddition
  test "left shift" $ quickCheck propLeftShift
  test "right shift" $ quickCheck propRightShift

propToStringLength :: ArbNibble -> Result
propToStringLength (ArbNibble n) = 4 === length (toBinString n)

propHasBinDigits :: ArbNibble -> Result
propHasBinDigits (ArbNibble n) = (all (\d -> d == '1' || d == '0') $ toCharArray (toBinString n))
  <?> "String representation of Nibble contains not only digits 1 and 0"

propStringRoundtrip :: ArbNibble -> Result
propStringRoundtrip (ArbNibble n) = fromBinString (toBinString n) === Just n

propAddition :: ArbNibble -> ArbNibble -> Result
propAddition (ArbNibble a) (ArbNibble b) =
  case add a b of
    (Overflow (Bit true) _) ->
      let c = S.add <$> toInt a <*> toInt b
      in c > Just 15 <?> "Unexpected overflow bit"
    (Overflow (Bit false) r) ->
      let c = S.add <$> toInt a <*> toInt b
      in c === toInt r

propLeftShift :: ArbNibble -> ArbBit -> Result
propLeftShift (ArbNibble n@(Nibble a b c d)) (ArbBit e) =
  let (Overflow a' (Nibble b' c' d' e')) = leftShift e n
  in [a, b, c, d, e] === [a', b', c', d', e']

propRightShift :: ArbBit -> ArbNibble -> Result
propRightShift (ArbBit a) (ArbNibble n@(Nibble b c d e)) =
  let (Overflow e' (Nibble a' b' c' d')) = rightShift a n
  in [a, b, c, d, e] === [a', b', c', d', e']
