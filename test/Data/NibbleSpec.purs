module Data.Nibble.Spec (spec, ArbNibble(..)) where

import Control.Monad.Eff.Random (RANDOM)
import Data.Bit (Overflow(..), Bit)
import Data.Bits (add, fromString, leftShift, rightShift, toInt, toString)
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Nibble (Nibble(..))
import Data.String (length, toCharArray)
import Test.QuickCheck (class Arbitrary, Result, (<?>), (===))
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)
import Prelude hiding (add)

newtype ArbNibble = ArbNibble Nibble
derive instance newtypeArbNibble :: Newtype ArbNibble _
derive newtype instance eqArbNibble :: Eq ArbNibble
derive newtype instance showArbNibble :: Show ArbNibble

instance arbitraryNibble :: Arbitrary ArbNibble where
  arbitrary = ArbNibble <$> (
    Nibble <$> arbitrary
           <*> arbitrary
           <*> arbitrary
           <*> arbitrary
  )

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = suite "Nibble" do
  test "toString has length 4" $ quickCheck propToStringLength
  test "toString contains only 0 and 1" $ quickCheck propHasBinDigits
  test "toString >>> fromString" $ quickCheck propStringRoundtrip
  test "addition works like Int" $ quickCheck propAddition
  test "left shift" $ quickCheck propLeftShift
  test "right shift" $ quickCheck propRightShift

propToStringLength :: ArbNibble -> Result
propToStringLength (ArbNibble n) = 4 === length (toString n)

propHasBinDigits :: ArbNibble -> Result
propHasBinDigits (ArbNibble n) = (all (\d -> d == '1' || d == '0') $ toCharArray (toString n))
  <?> "String representation of Nibble contains not only digits 1 and 0"

propStringRoundtrip :: ArbNibble -> Result
propStringRoundtrip (ArbNibble n) = fromString (toString n) === Just n

propAddition :: ArbNibble -> ArbNibble -> Result
propAddition (ArbNibble a) (ArbNibble b) =
  case add a b of
    (Overflow true _) -> (toInt a + toInt b) > 15 <?> "Unexpected overflow bit"
    (Overflow false r) -> toInt a + toInt b === toInt r

propLeftShift :: ArbNibble -> Bit -> Result
propLeftShift (ArbNibble n@(Nibble a b c d)) e =
  let (Overflow a' (Nibble b' c' d' e')) = leftShift e n
  in [a, b, c, d, e] === [a', b', c', d', e']

propRightShift :: Bit -> ArbNibble -> Result
propRightShift a (ArbNibble n@(Nibble b c d e)) =
  let (Overflow e' (Nibble a' b' c' d')) = rightShift a n
  in [a, b, c, d, e] === [a', b', c', d', e']
