import Control.Monad
import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.Test

import Logic

main :: IO ()
main = do
  result <- quickCheckResult $ conjoin properties
  unless (isSuccess result) exitFailure

properties :: [Property]
properties =
  [ counterexample "test case 01" $
      twice "hello" == "hellohello"
  ]
