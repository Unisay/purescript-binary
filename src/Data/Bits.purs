module Data.Bits
  ( Bits(..)
  , makeBits
  , reverseBits
  , firstBit
  , lastBit
  , bitsArray
  , bitsLength
  , stripLeadingZeros
  , intToBits
  , zeroWiden
  ) where

import Prelude

import Control.Plus (empty)
import Data.Array ((:))
import Data.Array as A
import Data.Bit (Bit(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty(..), fromNonEmpty, (:|))

newtype Bits = Bits (NonEmpty Array Bit)
derive instance newtypeBits :: Newtype Bits _
derive newtype instance eqBits :: Eq Bits
derive newtype instance ordBits :: Ord Bits
instance showBits :: Show Bits where
  show = bitsArray >>> show
instance semigroupBits :: Semigroup Bits where
  append (Bits (NonEmpty a as)) bits = Bits (a :| as <> bitsArray bits)


makeBits :: Array Bit -> Maybe Bits
makeBits [] = Nothing
makeBits bs = Bits <$> (NonEmpty <$> A.head bs <*> A.tail bs)

reverseBits :: Bits -> Bits
reverseBits bits@(Bits (NonEmpty h t)) =
  Bits (last :| A.reverse rest) where
    last = lastBit bits
    rest =  fromMaybe [h] (A.init (h : t))

firstBit :: Bits -> Bit
firstBit (Bits (NonEmpty h _)) = h

lastBit :: Bits -> Bit
lastBit (Bits (NonEmpty h [])) =  h
lastBit (Bits (NonEmpty h t)) = fromMaybe h (A.last t)

bitsArray :: Bits -> Array Bit
bitsArray (Bits bits) = fromNonEmpty (:) bits

bitsLength :: Bits -> Int
bitsLength (Bits (NonEmpty _ t)) = A.length t + 1

stripLeadingZeros :: Bits -> Bits
stripLeadingZeros (Bits (NonEmpty (Bit b) tail)) | not b, (Just { head: hd, tail: tl }) <- A.uncons tail =
  stripLeadingZeros (Bits (hd :| tl))
stripLeadingZeros bits = bits

zeroWiden :: Int -> Bits -> Bits
zeroWiden w bits =
  if d < 1 then bits else Bits (Bit false :| A.replicate (sub d 1) (Bit false)) <> bits
    where d = sub w (bitsLength bits)

singleBit :: Boolean -> Bits
singleBit b = Bits (Bit b :| [])

-- | Converts a non-negative `Int` value into an `Array Bit`
intToBits :: Int -> Bits
intToBits = intToBits' >>> stripLeadingZeros where
  intToBits' 0 = Bits (Bit false :| empty)
  intToBits' n | n `mod` 2 == 1 = (intToBits (n `div` 2)) <> (singleBit true)
               | otherwise = (intToBits (n `div` 2)) <> (singleBit false)
