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
      times "2\nhello" == "hellohello"
  , counterexample "test case 02" $
      times "3\nlol" == "lollollol"
  , counterexample "test case 03" $
      times "0\nsomething" == ""
  ]
