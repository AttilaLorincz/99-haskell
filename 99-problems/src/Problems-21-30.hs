{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.List (maximum, minimum, nub)
import System.Exit (exitFailure)
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Test
import Test.QuickCheck.Monadic

main :: IO ()
main = do
  result <- quickCheckResult $ conjoin tests
  unless (isSuccess result) exitFailure

tests = join
  [ tests_21
  , tests_22
  , tests_23
  , tests_24
  -- , tests_25
  -- , tests_26
  -- , tests_27
  -- , tests_28
  ]

-- Problem 21: Insert an element at a given position into a list.
tests_21 :: [Property]
tests_21 =
  [ counterexample "problem 21 test case 01" $
      insertAt 2 'X' "abcd" == "aXbcd"
  , counterexample "problem 21 generic" $
      \(e :: Int) list1 list2 ->
        insertAt (1 + length list1) e (list1 ++ list2) == list1 ++ [e] ++ list2
  ]

insertAt :: Int -> a -> [a] -> [a]
insertAt = undefined

-- Problem 22: Create a list containing all integers within a given range.
tests_22 :: [Property]
tests_22 =
  [ counterexample "problem 22 test case 01" $
      range 4 9 == [4, 5, 6, 7, 8, 9]
  , counterexample "problem 22 result length" $
      \a b -> length (range a b) == (1 + abs (a-b))
  , counterexample "problem 22 items distinct" $
      \a b -> range a b == nub (range a b)
  , counterexample "problem 22 maximum" $
      \a b -> maximum (range a b) == (max a b)
  , counterexample "problem 22 minimum" $
      \a b -> minimum (range a b) == (min a b)
  ]

range :: Int -> Int -> [Int]
range = undefined

-- Problem 23: Extract a given number of randomly selected elements from a list.
tests_23 :: [Property]
tests_23 =
  [ counterexample "problem 23 length" $
      test_rnd_select $ \n lst result -> length result == (min (length lst) n)
  , counterexample "problem 23 items" $
      test_rnd_select $ \n lst result -> all ((flip elem) lst) result
  ]
 where
  test_rnd_select check =
    \(pn :: Positive Int) (lst :: [Int]) -> monadicIO $ do
      let n = getPositive pn
      gen <- lift newStdGen
      let result = rnd_select gen lst n
      assert $ check n lst result

rnd_select :: (RandomGen g, Random a) => g -> [a] -> Int -> [a]
rnd_select = undefined

-- Problem 24: Lotto: Draw N different random numbers from the set 1..M.
tests_24 :: [Property]
tests_24 =
  [ counterexample "problem 24 length" $
      test_rnd_selnum $ \n m result -> length result == (min m n)
  , counterexample "problem 24 items in range" $
      test_rnd_selnum $ \_ m result -> all (\x -> x >= 1 && x <= m) result
  , counterexample "problem 24 items distinct" $
    test_rnd_selnum $ \_ _ result -> length result == length (nub result)
  ]
 where
  test_rnd_selnum check =
    \(pn :: Positive Int) (pm :: Positive Int) -> monadicIO $ do
      let n = getPositive pn
      let m = getPositive pm
      gen <- lift newStdGen
      let result = rnd_selnum gen n m
      assert $ check n m result

rnd_selnum :: (RandomGen g) => g -> Int -> Int -> [Int]
rnd_selnum = undefined

-- Problem 25: Generate a random permutation of the elements of a list.

-- Problem 26: (**) Generate the combinations of K distinct objects chosen from the N elements of a list

-- Problem 27: Group the elements of a set into disjoint subsets.

-- Problem 28: Sorting a list of lists according to length of sublists
