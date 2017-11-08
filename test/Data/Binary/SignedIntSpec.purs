module Data.Binary.SignedInt.Spec
  ( spec
  ) where

import Prelude

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (foldr, replicate)
import Data.Array as A
import Data.Binary.BaseN (bin, toStringAs)
import Data.Binary as Bin
import Data.Binary.SignedInt (fromInt, toInt, toNumberAs)
import Data.Foldable (all)
import Data.Int as Int
import Data.Newtype (unwrap)
import Data.String as Str
import Data.Typelevel.Num (class GtEq, class Pos, D32, d32, d99)
import Test.Arbitrary (ArbInt(..), ArbSignedInt32(ArbSignedInt32))
import Test.QuickCheck (Result(..), (<?>), (===))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM, console :: CONSOLE | e)
spec = suite "SignedInt" do
  test "fromInt" $ quickCheck (propFromInt d32)
  test "fromInt" $ quickCheck (propFromInt d99)
  test "number of bits is 32" $ quickCheck propBitSize
  test "negation" $ quickCheck propNegation
  test "propIntRoundtrip" $ quickCheck propIntRoundtrip
  test "toBinString contains only bin digits" $ quickCheck propBinString
  test "toBinString isn't empty" $ quickCheck propBinStringEmptiness
  test "toBinString produces unique representation" $ quickCheck propBinStringUniqness
  test "addition" $ quickCheck propAddition
  test "multiplication" $ quickCheck propMultiplication

propFromInt :: ∀ b . Pos b => GtEq b D32 => b -> ArbInt -> Result
propFromInt b (ArbInt i) =
  expected == actual
    <?> "\nExpected:  " <> show expected
    <>  "\nActual:    " <> show actual
    <>  "\nInt:       " <> show i
    <>  "\nSignedInt: " <> show si
  where
    expected = Int.toStringAs Int.binary i
    actual = toNumberAs bin si
    si = fromInt b i

propBitSize :: ArbSignedInt32 -> Result
propBitSize (ArbSignedInt32 si) =
  expected == actual
    <?> "\nExpected:  " <> show expected
    <>  "\nActual:    " <> show actual
    <>  "\nSignedInt: " <> show si
  where
    expected = 32
    actual = Bin.length (Bin.toBits si)

propNegation :: ArbSignedInt32 -> Result
propNegation (ArbSignedInt32 si) =
  si === (foldr compose id (replicate 8 negate) $ si)

propIntRoundtrip :: ArbInt -> Result
propIntRoundtrip (ArbInt i) = i === i' where
  i' = toInt si
  si = fromInt d32 i

propBinString :: ArbSignedInt32 -> Result
propBinString (ArbSignedInt32 ui) =
  let x = toStringAs bin ui
  in all (\d -> d == '1' || d == '0') (Str.toCharArray x)
    <?> "String representation of SignedInt contains not only digits 1 and 0: " <> x

propBinStringEmptiness :: ArbSignedInt32 -> Result
propBinStringEmptiness (ArbSignedInt32 ui) =
  not Str.null (toStringAs bin ui)
    <?> "String representation of SignedInt must not be empty"

propBinStringUniqness :: Array ArbSignedInt32 -> Result
propBinStringUniqness as = A.length sts === A.length uis where
  sts = A.nub $ map (toStringAs bin) uis
  uis = A.nub $ map unwrap as

propAddition :: ArbInt -> ArbInt -> Result
propAddition (ArbInt a) (ArbInt b) =
  expected == actual
    <?> "\nExpected:          " <> show expected
    <>  "\nActual:            " <> show actual
    <>  "\nInt (left):        " <> show a
    <>  "\nInt (right):       " <> show b
    <>  "\nSignedInt (left):  " <> show (si a)
    <>  "\nSignedInt (right): " <> show (si b)
    <>  "\nSignedInt (sum):   " <> show sum
  where
    expected = a + b
    actual = toInt sum
    sum = si a + si b
    si = fromInt d32

propMultiplication :: ArbInt -> ArbInt -> Result
propMultiplication (ArbInt a) (ArbInt b) | a == top && b == top =
  Success -- | PS wraps around Int incorrectly in this case
propMultiplication (ArbInt a) (ArbInt b) =
  expected == actual
    <?> "\nExpected:          " <> show expected
    <>  "\nActual:            " <> show actual
    <>  "\nInt (a):           " <> show a
    <>  "\nInt (b):           " <> show b
    <>  "\nSignedInt (a):     " <> show (si a)
    <>  "\nSignedInt (b):     " <> show (si b)
    <>  "\nSignedInt (mul):   " <> show res
  where
    actual = toInt res
    expected = a * b
    res = si a * si b
    si = fromInt d32
