module Data.Binary.Int.Spec (spec) where


import Control.Monad.Eff.Random (RANDOM)
import Data.Array (reverse, snoc, (:))
import Data.Binary (Bit(Bit), isNegative, tryFromBits)
import Data.Binary as Bin
import Data.Binary.Arbitrary (ArbBit(..), ArbBits32(..), ArbInt(ArbInt))
import Data.Binary.Overflow (Overflow(Overflow))
import Data.Int.Bits as Int
import Data.List.Lazy (iterate, take, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Ord (lessThan)
import Data.Tuple (Tuple(..), fst, uncurry)
import Prelude hiding (add)
import Test.QuickCheck (Result, (<?>))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = suite "Binary Int" do
  test "and" $ quickCheck propAnd
  test "xor" $ quickCheck propXor
  test "or" $ quickCheck propOr
  test "invert" $ quickCheck propInvert
  test "add" $ quickCheck propAdd
  test "leftShift" $ quickCheck propLeftShift
  test "rightShift" $ quickCheck propRightShift
  test "tryFromBits" $ quickCheck prop32BitsToInt

propAnd :: ArbInt -> ArbInt -> Result
propAnd (ArbInt a) (ArbInt b) =
  expected == actual
    <?> "\nExpected: " <> show expected
    <>  "\nActual:   " <> show actual
  where
    expected = Int.and a b
    actual   = Bin.and a b

propXor :: ArbInt -> ArbInt -> Result
propXor (ArbInt a) (ArbInt b) =
  expected == actual
    <?> "\nExpected: " <> show expected
    <>  "\nActual:   " <> show actual
  where
    expected = Int.xor a b
    actual   = Bin.xor a b

propOr :: ArbInt -> ArbInt -> Result
propOr (ArbInt a) (ArbInt b) =
  expected == actual
    <?> "\nExpected: " <> show expected
    <>  "\nActual:   " <> show actual
  where
    expected = Int.or a b
    actual   = Bin.or a b

propInvert :: ArbInt -> Boolean
propInvert (ArbInt i) =
  Bin.invert i /= i && (Bin.invert (Bin.invert i)) == i

propAdd :: ArbBit -> ArbInt -> ArbInt -> Result
propAdd (ArbBit bit@(Bit x)) (ArbInt a) (ArbInt b) =
  expected == actual
    <?> "\nExpected: " <> show expected
    <>  "\nActual:   " <> show actual
    <>  "\nLeft:     " <> show a
    <>  "\nRight:    " <> show b
  where
    expected = a + b
    actual   = Bin.unsafeAdd a b

propLeftShift :: ArbBit -> ArbInt -> Result
propLeftShift (ArbBit bit) (ArbInt int) =
  expected == actual
    <?> "\nExpected: " <> show expected
    <>  "\nActual:   " <> show actual
    <>  "\nInt:      " <> show int
  where
    expected = bit : unwrap (Bin.toBits int)
    actual = toUnfoldable (take 33 $ map fst results)
    results = iterate (uncurry Bin.leftShift <#> \(Overflow a b) -> Tuple a b) (Tuple bit int)

propRightShift :: ArbBit -> ArbInt -> Result
propRightShift (ArbBit bit) (ArbInt int) =
  expected == actual
    <?> "\nExpected: " <> show expected
    <>  "\nActual:   " <> show actual
    <>  "\nInt:      " <> show int
  where
    expected = unwrap (Bin.toBits int) `snoc` bit
    actual = reverse $ toUnfoldable (take 33 $ map fst results)
    results = iterate (uncurry Bin.rightShift <#> \(Overflow a b) -> Tuple a b) (Tuple bit int)

-- TODO

prop32BitsToInt :: ArbBits32 -> Result
prop32BitsToInt (ArbBits32 bits) =
  expected == actual
    <?> "\nExpected: " <> show expected
    <>  "\nActual:   " <> show actual
    <>  "\nResult:   " <> show result
    <>  "\nBits:     " <> show bits
  where
    expected = Just (isNegative bits)
    actual = flip lessThan zero <$> result
    result :: Maybe Int
    result = tryFromBits bits
