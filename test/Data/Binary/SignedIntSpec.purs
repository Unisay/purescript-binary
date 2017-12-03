module Data.Binary.SignedInt.Spec
  ( spec
  ) where

import Test.Arbitrary

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (foldr, replicate)
import Data.Array as A
import Data.Binary as Bin
import Data.Binary.BaseN (Radix(..), fromStringAs, toStringAs)
import Data.Binary.SignedInt (SignedInt, asBits, fromInt, toInt, toString2c, tryAsBits)
import Data.Foldable (all)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String as Str
import Data.Typelevel.Num (class Gt, class GtEq, class Pos, D2, D32, D42, d32, d99)
import Imul (imul)
import Prelude (compose, discard, id, map, negate, not, show, zero, ($), (*), (+), (<>), (==), (||))
import Test.QuickCheck (Result, (<?>), (===))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM, console :: CONSOLE | e)
spec = suite "SignedInt" do
  test "number of bits" $ quickCheck propNumberOfBits
  test "fromInt 32" $ quickCheck (propFromInt d32)
  test "fromInt 99" $ quickCheck (propFromInt d99)
  test "number of bits is 32" $ quickCheck propBitSize
  test "expanding bits doesn't loose data" $ quickCheck propBitExpansion
  test "negation" $ quickCheck propNegation
  test "toBinString contains only bin digits" $ quickCheck propBinString
  test "toBinString isn't empty" $ quickCheck propBinStringEmptiness
  test "toBinString produces unique representation" $ quickCheck propBinStringUniqness
  test "addition" $ quickCheck propAddition
  test "multiplication" $ quickCheck propMultiplication
  test "Int roundtrip" $ quickCheck propIntRoundtrip
  test "String roundtrip" $ quickCheck propStringRoundtrip

propNumberOfBits :: List ArbSignedInt32 ->
                    List (ArbSemiringOp (SignedInt D32)) ->
                    Result
propNumberOfBits ints ops =
  expected == actual
    <?> "\nExpected:  " <> show expected
    <>  "\nActual:    " <> show actual
    <>  "\nInts:      " <> show ints
    <>  "\nOps:       " <> show ops
    <>  "\nRes:       " <> show res
  where
    expected = 32
    actual = Bin.length $ Bin.toBits res
    res = r ints ops zero
    r Nil _ a = a
    r _ Nil a = a
    r ((ArbSignedInt32 i):is) ((ArbSemiringOp _ o):os) a = r is os (i `o` a)

propFromInt :: ∀ b . Pos b => Gt b D2 => GtEq b D32 => b -> ArbInt -> Result
propFromInt b (ArbInt i) =
  expected == actual
    <?> "\nExpected:  " <> show expected
    <>  "\nActual:    " <> show actual
    <>  "\nInt:       " <> show i
    <>  "\nSignedInt: " <> show si
  where
    expected = Int.toStringAs Int.binary i
    actual = toStringAs Bin si
    si = fromInt b i

propBitExpansion :: ArbSignedInt32 -> Result
propBitExpansion (ArbSignedInt32 i) =
  expected == actual
    <?> "\nExpected:   " <> show expected
    <>  "\nActual:     " <> show actual
    <>  "\nSignedInt:  " <> show i
  where
    expected = Just i
    actual :: Maybe (SignedInt D32)
    actual = tryAsBits expanded
    expanded :: SignedInt D42
    expanded = asBits i

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
  expected == actual
    <?> "\nExpected:  " <> show expected
    <>  "\nActual:    " <> show actual
    <>  "\nSignedInt: " <> show si
  where
    expected = si
    actual = foldr compose id (replicate 8 negate) $ si

propIntRoundtrip :: ArbInt -> Result
propIntRoundtrip (ArbInt i) = i === i' where
  i' = toInt si
  si = fromInt d32 i

propBinString :: ArbSignedInt32 -> Result
propBinString (ArbSignedInt32 ui) =
  let x = toString2c Bin ui
  in all (\d -> d == '1' || d == '0') (Str.toCharArray x)
    <?> "String representation of SignedInt contains not only digits 1 and 0: " <> x

propBinStringEmptiness :: ArbSignedInt32 -> Result
propBinStringEmptiness (ArbSignedInt32 ui) =
  not Str.null (toString2c Bin ui)
    <?> "String representation of SignedInt must not be empty"

propBinStringUniqness :: Array ArbSignedInt32 -> Result
propBinStringUniqness as = A.length sts === A.length uis where
  sts = A.nub $ map (toString2c Bin) uis
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
    expected = a `imul` b
    res = si a * si b
    si = fromInt d32

propStringRoundtrip :: ArbSignedInt32 -> ArbRadix -> Result
propStringRoundtrip (ArbSignedInt32 i) (ArbRadix radix) =
  expected == actual
    <?> "\nExpected:  " <> show expected
    <>  "\nActual:    " <> show actual
  where
    expected = Just i
    actual :: Maybe (SignedInt D32)
    actual = fromStringAs radix s
    s = toStringAs radix i
