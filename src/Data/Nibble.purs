module Data.Nibble
  ( Nibble (..)
  ) where

import Data.Typelevel.Num as N
import Control.Apply ((<$>), (<*>))
import Data.Bit (Bit, Overflow(Overflow), bitToString, charToBit)
import Data.Bits (class Bits, toString)
import Data.BooleanAlgebra (not)
import Data.Eq (class Eq)
import Data.Function ((>>>))
import Data.Int (pow)
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord, compare)
import Data.Semigroup ((<>))
import Data.Semiring ((*), (+))
import Data.Show (class Show)
import Data.String (toCharArray)
import Data.Typelevel.Num (D4)

data Nibble = Nibble Bit Bit Bit Bit

derive instance eqNibble :: Eq Nibble

instance showNibble :: Show Nibble where
  show n = toString n

instance ordNibble :: Ord Nibble where
  compare l r = compare (toString l) (toString r)

instance bitsNibble :: Bits D4 Nibble where
  invert (Nibble a b c d) = Nibble (not a) (not b) (not c) (not d)

  -- | Unsigned binary addition
  -- | Accepts a carry-over bit from the previous addition
  -- | Returns resulting `Nibble` with overflow bit
  add' b (Nibble a0 b0 c0 d0) (Nibble a1 b1 c1 d1) =
    let (Overflow d' d) = addBits d0 d1 b
        (Overflow c' c) = addBits c0 c1 d'
        (Overflow b' b) = addBits b0 b1 c'
        (Overflow a' a) = addBits a0 a1 b'
    in Overflow a' (Nibble a b c d) where

    addBits :: Bit -> Bit -> Bit -> Overflow Bit
    addBits false false false = Overflow false false
    addBits false false true  = Overflow false true
    addBits false true  false = Overflow false true
    addBits false true  true  = Overflow true false
    addBits true  false false = Overflow false true
    addBits true  false true  = Overflow true false
    addBits true  true  false = Overflow true false
    addBits true  true  true  = Overflow true true

  zero = Nibble false false false false
  one = Nibble false false false true

  leftShift z (Nibble a b c d) = Overflow a (Nibble b c d z)
  rightShift z (Nibble a b c d) = Overflow d (Nibble z a b c)

  toString (Nibble a b c d) =
    bitToString a <> bitToString b <> bitToString c <> bitToString d

  fromString = toCharArray >>> fromChars where
    fromChars cs =
      case cs of
      [a, b, c, d] -> Nibble <$> charToBit a
                             <*> charToBit b
                             <*> charToBit c
                             <*> charToBit d
      otherwise -> Nothing

  foldInt p (Nibble a b c d) =
    let i = N.toInt p
        bit = if _ then 1 else 0
        base = pow 2
    in (base (i + 0)) * (bit d)
     + (base (i + 1)) * (bit c)
     + (base (i + 2)) * (bit b)
     + (base (i + 3)) * (bit a)
