module Data.Binary.Convert
  ( tryFromSigned
  ) where

import Data.Binary as Bin
import Data.Binary.SignedInt (SignedInt, isNegative)
import Data.Binary.UnsignedInt (UnsignedInt)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (class Pos)
import Prelude (($))

tryFromSigned :: ∀ b . Pos b => SignedInt b -> Maybe (UnsignedInt b)
tryFromSigned s | isNegative s = Nothing
tryFromSigned s = Bin.tryFromBits $ Bin.stripLeadingZeros $ Bin.toBits s
