module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Prelude hiding (add)
import Data.Binary.Bits.Spec as Bits
import Data.Binary.Int.Spec as Int

main :: ∀ e. Eff ( console     :: CONSOLE
                 , testOutput  :: TESTOUTPUT
                 , avar        :: AVAR
                 , random      :: RANDOM
                 | e
                 ) Unit
main = runTest do
  Bits.spec
  Int.spec
