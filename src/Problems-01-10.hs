import Control.Monad
import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.Test

main :: IO ()
main = do
  result <- quickCheckResult $ conjoin tests
  unless (isSuccess result) exitFailure

tests = join
  [ tests_01
  , tests_02
  -- add tests of further problems here
  ]

-- Problem 1: Find the last element of a list.
tests_01 :: [Property]
tests_01 =
  [ counterexample "problem 01 test case 01" $
      myLast [1, 2, 3, 4] == 4
  , counterexample "problem 01 test case 02" $
      myLast "xyz" == 'z'
  ]

--myLast :: [a] -> a
myLast (head : []) = head
myLast (head : tail) = myLast tail
myLast [] = error "tail of empty list"

-- Problem 2: Find the last but one element of a list.
tests_02 :: [Property]
tests_02 =
  [ counterexample "problem 02 test case 01" $
      myButLast [1, 2, 3, 4] == 3
  , counterexample "problem 02 test case 02" $
      myButLast "xyz" == 'y'
  , counterexample "problem 02 test case 03" $
      myButLast "" == error "less than 2 elements"
  ]

myButLast :: [a] -> a
myButLast (head : (second:[])) = head
myButLast (head : tail) = myButLast tail
myButLast [] = error "less than 2 elements"
