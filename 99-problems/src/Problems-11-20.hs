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
encodeModified xs = [tupletorun e | e <- encode xs] where 
	tupletorun (1,x) = Single x 
	tupletorun (n,x) = Multiple n x
	encode :: (Eq a) => [a] -> [(Int, a)]
	encode l = [(length i, head i) | i<- pack l ] where 
		pack :: (Eq a) => [a] -> [[a]]
		pack [] = []	
		pack (x:xs) = reverse $ packl (x:xs) [] where
			packl :: (Eq a) => [a] -> [[a]] -> [[a]]
			packl [] acc = acc 
			packl (x:xs) acc 
					| (acc /= [] && x == head (head acc)) = packl xs $ (x : (head acc )) : (tail acc)
					| otherwise =  packl xs $ [x] : acc 


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
decodeModified [] = []
decodeModified (ra:ras) = runtolist ra ++ decodeModified ras where 
	runtolist (Single x) = [x]
--	runtolist (Multiple n x) = take n $ [x | _ <- [1,1..] ] 
	runtolist (Multiple n x) = replicate n x 

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
encodeDirect = foldr encodeDirectCount [] where
     encodeDirectCount x [] = [Single x]
     encodeDirectCount x (h@(Single b):rs)
			| x == b = (Multiple 2 x):rs
			| otherwise = (Single x):h:rs
     encodeDirectCount x (h@(Multiple n b):rs)
			| x == b = (Multiple (n+1) x):rs
			| otherwise = (Single x):h:rs
			
-- Problem 14: Duplicate the elements of a list.
tests_14 :: [Property]
tests_14 =
  [ counterexample "problem 14 test case 01" $
      dupli [1, 2, 3] == [1, 1, 2, 2, 3, 3]
  ]

dupli :: [a] -> [a]
dupli = repli 2

-- Problem 15: Replicate the elements of a list a given number of times.
tests_15 :: [Property]
tests_15 =
  [ counterexample "problem 15 test case 01" $
      repli 3 "abc" == "aaabbbccc"
  ]

repli :: Int -> [a] -> [a]
repli n loa = concat[replicate n i | i <- loa] 

-- Problem 16: Drop every Nth element from a list.
tests_16 :: [Property]
tests_16 =
  [ counterexample "problem 16 test case 01" $
      dropEvery 3 "abcdefghik" == "abdeghk"
  ]

dropEvery :: Int -> [a] -> [a]
dropEvery n loa = [ i | (c, i) <- ( zip [1,2..] loa), (c `mod` n) /= 0]

-- Problem 17: Split a list into two parts; the length of the first part is given.
tests_17 :: [Property]
tests_17 =
  [ counterexample "problem 17 test case 01" $
      split 3 "abcdefghik" == ("abc", "defghik")
  ]

split :: Int -> [a] -> ([a], [a])
split n loa = (take n loa, drop n loa) 

-- Problem 18: Extract a slice from a list.
tests_18 :: [Property]
tests_18 =
  [ counterexample "problem 18 test case 01" $
      slice 2 7 "abcdefghik" == "cdefg"
  ]

slice :: Int -> Int -> [a] -> [a]
slice from n = drop from . take n

-- Problem 19: Rotate a list N places to the left.
tests_19 :: [Property]
tests_19 =
  [ counterexample "problem 19 test case 01" $
      rotate 3 "abcdefgh" == "defghabc"
  , counterexample "problem 19 test case 02" $
      rotate (-2) "abcdefgh" == "ghabcdef"
  ]

rotate :: Int -> [a] -> [a]
rotate n loa = do 
	let l = length loa
	let m = if n < 0 then l+n else n
	rotated <- (drop m loa) ++ (take m loa)
	return rotated

-- Problem 20: Remove the Kth element from a list.
tests_20 :: [Property]
tests_20 =
  [ counterexample "problem 20 test case 01" $
      removeAt 1 "abcd" == ('b', "acd")
  ]

removeAt :: Int -> [a] -> (a, [a])
removeAt k = (\(before, afterAndItem) -> (head afterAndItem, before ++ tail afterAndItem)) . splitAt (k)