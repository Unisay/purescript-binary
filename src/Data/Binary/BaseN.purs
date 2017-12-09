module Data.Binary.BaseN
  ( class BaseN
  , toStringAs
  , fromStringAs
  , Radix(..)
  , toBits
  , toInt
  , alphabet
  ) where

import Prelude

import Data.Binary.Bit (_0, _1)
import Data.Binary.Bits (Bits(Bits))
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

class BaseN a where
  toStringAs :: Radix -> a -> String
  fromStringAs :: Radix -> String -> Maybe a


-- | The number of unique digits (including zero) used to represent integers in
-- | a specific base.

data Radix = Bin | Oct | Dec | Hex

derive instance eqRadix :: Eq Radix
instance showRadix :: Show Radix where
  show Bin = "Binary"
  show Oct = "Octal"
  show Dec = "Decimal"
  show Hex = "Hexadecimal"

toBits :: Radix -> Bits
toBits Bin = Bits [_1, _0]
toBits Oct = Bits [_1, _0, _0, _0]
toBits Dec = Bits [_1, _0, _1, _0]
toBits Hex = Bits [_1, _0, _0, _0, _0]

toInt :: Radix -> Int
toInt Bin = 2
toInt Oct = 8
toInt Dec = 10
toInt Hex = 16

alphabet :: Radix -> Map Char Bits
alphabet Bin = fromFoldable [ Tuple '0' (Bits [_0]), Tuple '1' (Bits [_1]) ]
alphabet Oct = alphabet Bin <> fromFoldable [ Tuple '2' $ Bits [_1,_0]
                                            , Tuple '3' $ Bits [_1,_1]
                                            , Tuple '4' $ Bits [_1,_0,_0]
                                            , Tuple '5' $ Bits [_1,_0,_1]
                                            , Tuple '6' $ Bits [_1,_1,_0]
                                            , Tuple '7' $ Bits [_1,_1,_1]
                                            ]
alphabet Dec = alphabet Oct <> fromFoldable [ Tuple '8' $ Bits [_1,_0,_0,_0]
                                            , Tuple '9' $ Bits [_1,_0,_0,_1]
                                            ]
alphabet Hex = alphabet Dec <> fromFoldable [ Tuple 'a' $ Bits [_1,_0,_1,_0]
                                            , Tuple 'b' $ Bits [_1,_0,_1,_1]
                                            , Tuple 'c' $ Bits [_1,_1,_0,_0]
                                            , Tuple 'd' $ Bits [_1,_1,_0,_1]
                                            , Tuple 'e' $ Bits [_1,_1,_1,_0]
                                            , Tuple 'f' $ Bits [_1,_1,_1,_1]
                                            ]
