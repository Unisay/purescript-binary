module Data.Binary.UnsignedInt.Spec
  ( spec
  ) where

import Prelude

import Control.Monad.Eff.Random (RANDOM)
import Data.Array as A
import Data.Binary.BaseN (Radix(..), fromStringAs, toStringAs)
import Data.Binary.UnsignedInt (UnsignedInt, asBits, fromInt, toInt, tryAsBits)
import Data.Foldable (all)
import Data.Int (toNumber)
import Data.Int as Int
import Data.Maybe (Maybe(Just))
import Data.Newtype (unwrap)
import Data.String as Str
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (class GtEq, class Pos, D42, d31, d32, d99)
import Data.Typelevel.Num.Aliases (D31)
import Test.Arbitrary (ArbNonNegativeInt(ArbNonNegativeInt), ArbRadix(ArbRadix), ArbUnsignedInt31(ArbUnsignedInt31), NonOverflowingMultiplicands(NonOverflowingMultiplicands))
import Test.QuickCheck (Result, (<?>), (===))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = suite "UnsignedInt" do
  test "fromInt 32" $ quickCheck (propFromInt d32)
  test "fromInt 99" $ quickCheck (propFromInt d99)
  test "toInt" $ quickCheck propToInt
  test "expanding bits doesn't loose data" $ quickCheck propBitExpansion
  test "toBinString contains only bin digits" $ quickCheck propBinString
  test "toBinString isn't empty" $ quickCheck propBinStringEmptiness
  test "toBinString produces unique representation" $ quickCheck propBinStringUniqness
  test "addition" $ quickCheck propAddition
  test "multiplication" $ quickCheck propMultiplication
  test "baseN roundtrip" $ quickCheck propBaseNRoundtrip


propFromInt :: ∀ b . Pos b => GtEq b D31 => b -> ArbNonNegativeInt -> Result
propFromInt b (ArbNonNegativeInt i) =
  expected === actual where
    expected = Int.toStringAs Int.binary i
    actual = Str.dropWhile (eq '0') (toStringAs Bin (fromInt b i))

propToInt :: ArbUnsignedInt31 -> Result
propToInt (ArbUnsignedInt31 ui) =
  expected === actual where
    expected = Str.dropWhile (eq '0') (toStringAs Bin ui)
    actual = Int.toStringAs Int.binary (toInt ui)

propBitExpansion :: ArbUnsignedInt31 -> Result
propBitExpansion (ArbUnsignedInt31 ui) =
  expected == actual
    <?> "\nExpected:   " <> show expected
    <>  "\nActual:     " <> show actual
    <>  "\nUnsignedInt:" <> show ui
  where
    expected = Just ui
    actual :: Maybe (UnsignedInt D31)
    actual = tryAsBits expanded
    expanded :: UnsignedInt D42
    expanded = asBits ui

propBinString :: ArbUnsignedInt31 -> Result
propBinString (ArbUnsignedInt31 ui) =
  let x = toStringAs Bin ui
  in all (\d -> d == '1' || d == '0') (Str.toCharArray x)
    <?> "String representation of UnsignedInt contains not only digits 1 and 0: " <> x

propBinStringEmptiness :: ArbUnsignedInt31 -> Result
propBinStringEmptiness (ArbUnsignedInt31 ui) =
  not Str.null (toStringAs Bin ui)
    <?> "String representation of UnsignedInt must not be empty"

propBinStringUniqness :: Array ArbUnsignedInt31 -> Result
propBinStringUniqness as = A.length sts === A.length uis where
  sts = A.nub $ map (toStringAs Bin) uis
  uis = A.nub $ map unwrap as

propAddition :: ArbNonNegativeInt -> ArbNonNegativeInt -> Result
propAddition (ArbNonNegativeInt a) (ArbNonNegativeInt b) =
  expected == actual
    <?> "\nExpected: " <> show expected
    <>  "\nActual:   " <> show actual
    <>  "\na:        " <> show a
    <>  "\nb:        " <> show b
  where
    expected = if (toNumber a) + (toNumber b) > toNumber (top :: Int)
               then negate (bottom - (a + b))
               else a + b
    actual = toInt unsigned
    unsigned = u a + u b
    u = fromInt d31

propMultiplication :: NonOverflowingMultiplicands -> Result
propMultiplication (NonOverflowingMultiplicands (Tuple a b)) =
  a * b === toInt (u a * u b) where u = fromInt d31

propBaseNRoundtrip :: ArbUnsignedInt31 -> ArbRadix -> Result
propBaseNRoundtrip (ArbUnsignedInt31 u) (ArbRadix radix) =
  expected == actual
    <?> "\nExpected:  " <> show expected
    <>  "\nActual:    " <> show actual
  where
    expected = Just u
    actual = fromStringAs radix s
    s = toStringAs radix u
