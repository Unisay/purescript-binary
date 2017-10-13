module Data.Bit
  ( Overflow(..)
  , Bit
  , discardOverflow
  , charToBit
  , bitToChar
  , bitToString
  ) where


import Data.Function ((>>>))
import Data.Maybe (Maybe(..))
import Data.String (singleton)


type Bit = Boolean

data Overflow a = Overflow Bit a

discardOverflow :: ∀ a. Overflow a -> a
discardOverflow (Overflow _ a) = a

charToBit :: Char -> Maybe Bit
charToBit '1' = Just true
charToBit '0' = Just false
charToBit _   = Nothing

bitToChar :: Bit -> Char
bitToChar true  = '1'
bitToChar false = '0'

bitToString :: Bit -> String
bitToString = bitToChar >>> singleton
