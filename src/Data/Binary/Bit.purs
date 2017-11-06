module Data.Binary.Bit
  ( Bit(..)
  , bitToChar
  , charToBit
  , bitToInt
  , intToBit
  , _0
  , _1
  ) where

import Prelude

import Data.Array as A
import Data.String as Str
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

newtype Bit = Bit Boolean
derive instance newtypeBit :: Newtype Bit _
derive newtype instance eqBit :: Eq Bit
derive newtype instance ordBit :: Ord Bit
derive newtype instance heytingAlgebraBit :: HeytingAlgebra Bit
derive newtype instance booleanAlgebraBit :: BooleanAlgebra Bit
derive newtype instance boundedBit :: Bounded Bit

instance showBit :: Show Bit where
  show = bitToChar >>> A.singleton >>> Str.fromCharArray

_0 :: Bit
_0 = Bit false

_1 :: Bit
_1 = Bit true

charToBit :: Char -> Maybe Bit
charToBit '1' = Just _1
charToBit '0' = Just _0
charToBit _   = Nothing

bitToChar :: Bit -> Char
bitToChar (Bit true)  = '1'
bitToChar (Bit false) = '0'

intToBit :: Int -> Bit
intToBit 1 = _1
intToBit _ = _0

bitToInt :: Bit -> Int
bitToInt (Bit true)  = 1
bitToInt (Bit false) = 0
