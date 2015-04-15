{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.Test

import ParserCombinators

main :: IO ()
main = do
  result <- quickCheckResult $ conjoin tests
  unless (isSuccess result) exitFailure

tests :: [Property]
tests =
  [ counterexample "empty matches precisely the empty string" $
      \str -> (runP empty str /= []) == (str == "")
  , counterexample "char matches precisely the single given character" $
      \c str -> (runP (char c) str /= []) == (str == [c])
  , counterexample "anyChar matches precisely one arbitrary character" $
      \str -> (runP anyChar str /= []) == (length str == 1)
  , counterexample "string matches precisely the given string" $
      \txt str -> (runP (string txt) str /= []) == (str == txt)
  ]
