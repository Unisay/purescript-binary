module Data.Bit
  ( Bit(..)
  , charToBit
  , bitToChar
  ) where

import Prelude

import Data.Array (singleton)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (fromCharArray)

newtype Bit = Bit Boolean
derive instance newtypeBit :: Newtype Bit _
derive newtype instance eqBit :: Eq Bit
derive newtype instance ordBit :: Ord Bit
derive newtype instance heytingAlgebraBit :: HeytingAlgebra Bit
derive newtype instance booleanAlgebraBit :: BooleanAlgebra Bit
derive newtype instance boundedBit :: Bounded Bit

instance showBit :: Show Bit where
  show = bitToChar >>> singleton >>> fromCharArray

charToBit :: Char -> Maybe Bit
charToBit '1' = Just (Bit true)
charToBit '0' = Just (Bit false)
charToBit _   = Nothing

bitToChar :: Bit -> Char
bitToChar (Bit true)  = '1'
bitToChar (Bit false) = '0'
