module Data.Binary.BaseN
  ( class BaseN
  , toBase
  ) where

import Data.Binary (class Binary, Radix)

class Binary a <= BaseN a where
  toBase :: Radix -> a -> String
