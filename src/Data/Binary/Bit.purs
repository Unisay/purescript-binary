module Data.Binary.Bit
  ( Bit(..)
  , bitToChar
  , charToBit
  , bitToInt
  , intToBit
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

charToBit :: Char -> Maybe Bit
charToBit '1' = Just (Bit true)
charToBit '0' = Just (Bit false)
charToBit _   = Nothing

bitToChar :: Bit -> Char
bitToChar (Bit true)  = '1'
bitToChar (Bit false) = '0'

intToBit :: Int -> Bit
intToBit 1 = (Bit true)
intToBit _ = (Bit false)

bitToInt :: Bit -> Int
bitToInt (Bit true)  = 1
bitToInt (Bit false) = 0
