module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Prelude hiding (add)
import Data.Binary.Byte.Spec as Byte
import Data.Binary.Nibble.Spec as Nibble
import Data.Binary.Bins.Spec as Bins

main :: ∀ e. Eff ( console     :: CONSOLE
                 , testOutput  :: TESTOUTPUT
                 , avar        :: AVAR
                 , random      :: RANDOM
                 | e
                 ) Unit
main = runTest do
  Nibble.spec
  Byte.spec
  Bins.spec
