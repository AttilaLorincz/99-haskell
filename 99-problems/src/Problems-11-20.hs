{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Applicative
import Control.Monad
import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.Test

main :: IO ()
main = do
  result <- quickCheckResult $ conjoin tests
  unless (isSuccess result) exitFailure

tests = join
  [ tests_11
  , tests_12
  , tests_13
  , tests_14
  , tests_15
  , tests_16
  , tests_17
  , tests_18
  , tests_19
  , tests_20
  ]

-- Problem 11: Modified run-length encoding.
tests_11 :: [Property]
tests_11 =
  [ counterexample "problem 11 test case 01" $
      encodeModified "aaaabccaadeeee" ==
      [ Multiple 4 'a', Single 'b', Multiple 2 'c'
      , Multiple 2 'a', Single 'd', Multiple 4 'e'
      ]
  ]

encodeModified :: (Eq a) => [a] -> [Run a]
encodeModified = undefined

data Run a
  = Single a
  | Multiple Int a
  deriving (Eq, Show)

-- Problem 12: Decode a run-length encoded list.
tests_12 :: [Property]
tests_12 =
  [ counterexample "problem 12 test case 01" $
      decodeModified
        [ Multiple 4 'a', Single 'b', Multiple 2 'c'
        , Multiple 2 'a', Single 'd', Multiple 4 'e'
        ] ==
      "aaaabccaadeeee"
  ]

decodeModified :: [Run a] -> [a]
decodeModified = undefined

-- Problem 13: Run-length encoding of a list (direct solution).
-- Don't explicitly create the sublists containing the
-- duplicates, as in problem 9, but only count them.
tests_13 :: [Property]
tests_13 =
  [ counterexample "problem 13 test case 01" $
      encodeDirect "aaaabccaadeeee" ==
      [ Multiple 4 'a', Single 'b', Multiple 2 'c'
      , Multiple 2 'a', Single 'd', Multiple 4 'e'
      ]
  ]

encodeDirect :: (Eq a) => [a] -> [Run a]
encodeDirect = undefined

-- Problem 14: Duplicate the elements of a list.
tests_14 :: [Property]
tests_14 =
  [ counterexample "problem 14 test case 01" $
      dupli [1, 2, 3] == [1, 1, 2, 2, 3, 3]
  ]

dupli :: [a] -> [a]
dupli = undefined

-- Problem 15: Replicate the elements of a list a given number of times.
tests_15 :: [Property]
tests_15 =
  [ counterexample "problem 15 test case 01" $
      repli 3 "abc" == "aaabbbccc"
  ]

repli :: Int -> [a] -> [a]
repli = undefined

-- Problem 16: Drop every Nth element from a list.
tests_16 :: [Property]
tests_16 =
  [ counterexample "problem 16 test case 01" $
      dropEvery 3 "abcdefghik" == "abdeghk"
  ]

dropEvery :: Int -> [a] -> [a]
dropEvery = undefined

-- Problem 17: Split a list into two parts; the length of the first part is given.
tests_17 :: [Property]
tests_17 =
  [ counterexample "problem 17 test case 01" $
      split 3 "abcdefghik" == ("abc", "defghik")
  ]

split :: Int -> [a] -> ([a], [a])
split = undefined

-- Problem 18: Extract a slice from a list.
tests_18 :: [Property]
tests_18 =
  [ counterexample "problem 18 test case 01" $
      slice 2 7 "abcdefghik" == "cdefg"
  ]

slice :: Int -> Int -> [a] -> [a]
slice = undefined

-- Problem 19: Rotate a list N places to the left.
tests_19 :: [Property]
tests_19 =
  [ counterexample "problem 19 test case 01" $
      rotate 3 "abcdefgh" == "defghabc"
  , counterexample "problem 19 test case 02" $
      rotate (-2) "abcdefgh" == "ghabcdef"
  ]

rotate :: Int -> [a] -> [a]
rotate = undefined

-- Problem 20: Remove the Kth element from a list.
tests_20 :: [Property]
tests_20 =
  [ counterexample "problem 20 test case 01" $
      removeAt 1 "abcd" == ('b', "acd")
  ]

removeAt :: Int -> [a] -> (a, [a])
removeAt = undefined
