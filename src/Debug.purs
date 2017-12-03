module Debug where

import Prelude (class Show)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

dbg :: ∀ a s . Show s => s -> a -> a
dbg s a = let _ = unsafePerformEff (logShow s) in a

dbgs :: ∀ s . Show s => s -> s
dbgs s = let _ = unsafePerformEff (logShow s) in s
