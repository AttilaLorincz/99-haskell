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
  [ tests_01
  , tests_02
  , tests_03
  , tests_04
  , tests_05
  , tests_06
  , tests_07
  , tests_08
  , tests_09
  , tests_10
  ]

-- Problem 1: Find the last element of a list.
tests_01 :: [Property]
tests_01 =
  [ counterexample "problem 01" $
      \(elem :: Int) list ->
        myLast (list ++ [elem]) == elem
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

-- Problem 3: Find the Kth element of a list. The first element in the list is number 0.
tests_03 :: [Property]
tests_03 =
  [ counterexample "problem 03" $
      \(elem :: Int) list_1 list_2 ->
        elementAt (length list_1) (list_1 ++ [elem] ++ list_2) == elem
  ]

elementAt :: Int -> [a] -> a
elementAt 0 (head:tail) = head
elementAt i (head:tail) = elementAt (i-1) tail

-- Problem 4: Find the number of elements of a list.
tests_04 :: [Property]
tests_04 =
  [ counterexample "problem 04, empty" $
      myLength [] == 0
  , counterexample "problem 04, singleton" $
      \(elem :: Int) ->
        myLength [elem] == 1
  , counterexample "problem 04, composite" $
      \(list_1 :: [Int]) list_2 ->
        myLength (list_1 ++ list_2) == myLength list_1 + myLength list_2
  ]

myLength :: [a] -> Int
myLength [] = 0
myLength (head:tail) = 1 + myLength tail

-- Problem 5: Reverse a list.
tests_05 :: [Property]
tests_05 =
  [ counterexample "problem 05, empty" $
      case myReverse [] of
        [] -> True
        _  -> False
  , counterexample "problem 05, non-empty" $
      \(elem :: Int) list ->
        myReverse (elem : list) == myReverse list ++ [elem]
  ]

myReverse :: [a] -> [a]
myReverse [] = [] 
myReverse (head:tail) = (myReverse tail) ++ [head] 
	
listRevFast :: [a] -> [a]
listRevFast l = _listRevFast l []
    where
        _listRevFast :: [a] -> [a] -> [a]
        _listRevFast [] l = l
        _listRevFast (x:xs) l = _listRevFast xs (x:l)
		
		
-- Problem 6: Find out whether a list is a palindrome.
tests_06 :: [Property]
tests_06 =
  [ counterexample "problem 06, empty" $
      isPalindrome ([] :: [Int])
  , counterexample "problem 06, non-empty" $
      \(elem_1 :: Int) elem_2 list ->
        isPalindrome ([elem_1] ++ list ++ [elem_2]) == (elem_1 == elem_2 && isPalindrome list)
  ]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome a = a  == myReverse a

-- Problem 7: Flatten a nested list structure.
tests_07 :: [Property]
tests_07 =
  [ counterexample "problem 07, empty" $
      case flatten (List []) of
        [] -> True
        _  -> False
  , counterexample "problem 07, singleton" $
      \(elem :: Int) ->
        flatten (Elem elem) == [elem]
  , counterexample "problem 07, head-list" $
      \(list :: NestedList Int) subList ->
        flatten (List (list : subList)) == flatten list ++ flatten (List subList)
  ]

flatten :: NestedList a -> [a]
flatten  = myReverse . flattenAcc [] 
  where
  flattenAcc acc (List []) = acc
  flattenAcc acc (Elem x)  = x:acc
  flattenAcc acc (List (x:xs)) = flattenAcc (flattenAcc acc x) (List xs)

data NestedList a
  = Elem a
  | List [NestedList a]

instance (Show a) => Show (NestedList a) where
  show = \case
    Elem x -> "Elem " ++ show x
    List xs -> "List " ++ show xs

instance (Arbitrary a) => Arbitrary (NestedList a) where
  arbitrary = genNestedListFromList =<< arbitrary
  shrink = shrinkNothing

genNestedListFromList :: [a] -> Gen (NestedList a)
genNestedListFromList = \case
  []  -> return $ List []
  [x] -> return $ Elem x
  xs  -> do
    n <- choose (0, length xs)
    let (xs_1, xs_2) = splitAt n xs
    List <$> mapM genNestedListFromList [xs_1, xs_2]

-- Problem 8: Eliminate consecutive duplicates of list elements.
tests_08 :: [Property]
tests_08 =
  [ counterexample "problem 08" $
      \(lens :: [Positive Int]) ->
        let
          runs = intRunsFromLens lens
        in compress (join $ listsFromRuns runs) == map snd runs
  ]

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) 
			| (xs /= [] && x == head xs) = compress xs
			| otherwise =  x: compress xs 

intRunsFromLens :: [Positive Int] -> [(Positive Int, Int)]
intRunsFromLens = flip zip [1..]

listsFromRuns :: [(Positive Int, a)] -> [[a]]
listsFromRuns = map $ \(len, x) -> replicate (getPositive len) x

-- Problem 9: Pack consecutive duplicates of list elements into sublists.
tests_09 :: [Property]
tests_09 =
  [ counterexample "problem 09" $
      \(lens :: [Positive Int]) ->
        let
          runs = intRunsFromLens lens
          lists = listsFromRuns runs
        in pack (join lists) == lists
  ]

pack :: (Eq a) => [a] -> [[a]]
pack [] = []	
pack (x:xs) = myReverse $ packl (x:xs) [] where
	packl :: (Eq a) => [a] -> [[a]] -> [[a]]
	packl [] acc = acc 
	packl (x:xs) acc 
			| (acc /= [] && x == head (head acc)) = packl xs $ (x : (head acc )) : (tail acc)
			| otherwise =  packl xs $ [x] : acc 


-- Problem 10: Run-length encoding of a list.
-- Consecutive duplicates of elements are encoded as pairs (N, E)
-- where N is the number of duplicates of the element E.
tests_10 :: [Property]
tests_10 =
  [ counterexample "problem 10" $
      \(lens :: [Positive Int]) ->
        let
          runs = intRunsFromLens lens
        in encode (join $ listsFromRuns runs) == map (\(len, x) -> (getPositive len, x)) runs
  ]

encode :: (Eq a) => [a] -> [(Int, a)]
encode l = [(length i, head i) | i<- pack l ]
