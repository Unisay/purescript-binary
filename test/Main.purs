module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Binary.Bits.Spec as Bits
import Data.Binary.UnsignedInt.Spec as UnsignedInt
import Data.Binary.SignedInt.Spec as SignedInt
import Prelude hiding (add)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: ∀ e. Eff ( console     :: CONSOLE
                 , testOutput  :: TESTOUTPUT
                 , avar        :: AVAR
                 , random      :: RANDOM
                 | e
                 ) Unit
main = runTest do
  Bits.spec
  UnsignedInt.spec
  SignedInt.spec
