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
  [ counterexample "empty matches precisely the empty String" $
      \str -> (runP empty str /= []) == (str == "")
  , counterexample "char matches precisely the single given Char" $
      \c str -> (runP (char c) str /= []) == (str == [c])
  , counterexample "anyChar matches precisely one arbitrary Char" $
      \str -> (runP anyChar str /= []) == (length str == 1)
  , counterexample "string matches precisely the given String" $
      \txt str -> (runP (string txt) str /= []) == (str == txt)
  , counterexample "anyString matches any String" $
      \str -> (runP anyString str == [str])
  ]
