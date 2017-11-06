module Data.Binary.BaseN
  ( class BaseN
  , toBase
  , Radix(Radix)
  , bin
  , oct
  , dec
  , hex
  , alphabet
  , unsafeBitsAsChars
  ) where

import Prelude

import Data.Array ((!!))
import Data.Array as A
import Data.Binary as Bin
import Data.Binary.Bit (_0, _1)
import Data.Binary.Bits (Bits(..))
import Data.Maybe (fromMaybe)

class BaseN a where
  toBase :: Radix -> a -> String


-- | The number of unique digits (including zero) used to represent integers in
-- | a specific base.

newtype Radix = Radix Bits

derive newtype instance eqRadix :: Eq Radix
derive newtype instance showRadix :: Show Radix

-- | The base-2 system.
bin :: Radix
bin = Radix (Bits [_1, _0])

-- | The base-8 system.
oct :: Radix
oct = Radix (Bits [_1, _0, _0, _0])

-- | The base-10 system.
dec :: Radix
dec = Radix (Bits [_1, _0, _1, _0])

-- | The base-16 system.
hex :: Radix
hex = Radix (Bits [_1, _0, _0, _0, _0])

alphabet :: Array Char
alphabet = [ '0' ,'1' ,'2' ,'3' ,'4' ,'5' ,'6' ,'7' ,'8' ,'9' , 'a' ,'b' ,'c' ,'d' ,'e' ,'f' ]

unsafeBitsAsChars :: Bits -> Array Char
unsafeBitsAsChars bits = fromMaybe [] $ A.singleton <$> (alphabet !! Bin.unsafeBitsToInt bits)
