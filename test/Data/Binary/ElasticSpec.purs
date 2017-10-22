module Data.Binary.Elastic.Spec where

import Prelude

import Control.Monad.Eff.Random (RANDOM)
import Data.Array as A
import Data.Binary.Arbitrary (ArbNonNegativeInt(ArbNonNegativeInt), NonOverflowingMultiplicands(..))
import Data.Binary.Bits (Bits(..))
import Data.Binary.Class (toBinString, tryToInt)
import Data.Binary.Elastic (addLeadingZeros, diff, divMod, double, extendAdd, fromInt, half, multiply)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Test.QuickCheck (Result(Success), (<?>), (===))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = suite "Elastic Binary" do
  test "addLeadingZeros" $ quickCheck propAddLeadingZeros
  test "double" $ quickCheck propDouble
  test "half"   $ quickCheck propHalf
  test "diff"   $ quickCheck propDiff
  test "div"    $ quickCheck propDiv
  test "mod"    $ quickCheck propMod
  test "add"    $ quickCheck propAdd
  test "mul"    $ quickCheck propMultiply

propAddLeadingZeros :: ArbNonNegativeInt -> ArbNonNegativeInt -> Result
propAddLeadingZeros (ArbNonNegativeInt a) (ArbNonNegativeInt b) =
  if len < b then len' === b else len === len'
  where
    bits@(Bits bs) = fromInt a :: Bits
    bits'@(Bits bs') = addLeadingZeros b bits
    len = A.length bs
    len' = A.length bs'

propDouble :: ArbNonNegativeInt -> Result
propDouble (ArbNonNegativeInt i) =
  if i < maxInt
  then Just (i * 2) === tryToInt (double bits)
  else Success
  where maxInt = (top :: Int) / 2
        bits = fromInt i :: Bits

propHalf :: ArbNonNegativeInt -> Result
propHalf (ArbNonNegativeInt i) =
  Just (i / 2) === tryToInt (half bits)
  where bits = fromInt i :: Bits

propDiff :: ArbNonNegativeInt -> ArbNonNegativeInt -> Result
propDiff (ArbNonNegativeInt a) (ArbNonNegativeInt b) =
  Just intRes == binRes
    <?> "diff " <> toBinString as <> " " <> toBinString bs <> " failed: "
                <> show binRes <> " /= " <> show intRes
  where
    intRes = abs (a - b)
    binRes = tryToInt (diff as bs)
    as = fromInt a :: Bits
    bs = fromInt b :: Bits

propDiv :: ArbNonNegativeInt -> ArbNonNegativeInt -> Result
propDiv (ArbNonNegativeInt a) (ArbNonNegativeInt b) =
  Just (a `div` b) === tryToInt dv where
    as = fromInt a :: Bits
    bs = fromInt b :: Bits
    (Tuple dv _) = divMod as bs

propMod :: ArbNonNegativeInt -> ArbNonNegativeInt -> Result
propMod (ArbNonNegativeInt a) (ArbNonNegativeInt b) =
  Just (a `mod` b) === tryToInt md where
    as = fromInt a :: Bits
    bs = fromInt b :: Bits
    (Tuple _ md) = divMod as bs

propAdd :: ArbNonNegativeInt -> ArbNonNegativeInt -> Result
propAdd (ArbNonNegativeInt a) (ArbNonNegativeInt b) =
  Just (a + b) === tryToInt res where
    as = fromInt a :: Bits
    bs = fromInt b :: Bits
    res = extendAdd as bs

propMultiply :: NonOverflowingMultiplicands -> Result
propMultiply (NonOverflowingMultiplicands (Tuple a b)) =
  toBinString ires === toBinString bres where
    as = fromInt a :: Bits
    bs = fromInt b :: Bits
    ires :: Bits
    ires = fromInt (a * b)
    bres = multiply as bs
