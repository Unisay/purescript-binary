module Data.BitArray
  ( class EncodeBinary
  , class DecodeBinary
  , class Binary
  , Bit
  , Bits(..)
  , encodeBinary
  , decodeBinary
  ) where


import Control.Plus (empty)
import Data.Array.Partial (head)
import Data.BooleanAlgebra (not)
import Data.Eq (class Eq)
import Data.Foldable (foldl)
import Data.Function ((>>>))
import Data.Functor (class Functor, map)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Semigroup (class Semigroup)
import Data.Show (class Show)
import Data.Typelevel.Num (D0, D1)


type Bit = Boolean

newtype Bits = Bits (NonEmpty Array Bit)
derive newtype instance eqBits :: Eq Bits
derive newtype instance showBits :: Show Bits
-- derive newtype instance semigroupBits :: Semigroup Bits

class EncodeBinary a where
  encodeBinary :: a -> Bits

class DecodeBinary a where
  decodeBinary :: Bits -> a

class (EncodeBinary a, DecodeBinary a) <= Binary a

instance encodeBinaryBoolean :: EncodeBinary Boolean where
  encodeBinary b = Bits (b :| empty)

instance decodeBinaryBoolean :: DecodeBinary Boolean where
  decodeBinary (Bits (NonEmpty b _)) = b

instance encodeBinaryInt :: EncodeBinary Int where
  encodeBinary i = Bits (true :| empty) -- TODO

-- instance decodeBinaryInt :: DecodeBinary Int where
--   decodeBinary (Bits (NonEmpty true _)) = 42
--   decodeBinary bits = decodeBinary ((invert >>> increment) bits)


-- increment :: Bits -> Bits
-- increment (Bits bs) = Bits (foldl f true bs) where
--   f true true   = Tuple2 false true
--   f true false  = Tuple2 true false
--   f false true  = Tuple2 true false
--   f false false = Tuple2 false false


invert :: Bits -> Bits
invert (Bits bs) = Bits (map not bs)
