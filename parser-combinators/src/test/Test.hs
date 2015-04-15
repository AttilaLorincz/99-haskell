{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Char (isDigit)
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
  , counterexample "anyDigit matches precisely digits" $
      \c -> (runP anyDigit [c] /= []) == isDigit c
  , counterexample "list anyDigit matches printed non-negative Ints" $
      \(i :: Int) -> (runP (list anyDigit) (show i) /= []) == (i >= 0)
  ]

-- SOME COMPOSITE PARSERS

{- The char function constructs a parser that matches (and yields)
the given Char, and fails when the input doesn't start with that.
-}
char :: Char -> Parser Char
char c = filterP (c==) anyChar

{- The string function constructs a parser that matches (and yields)
the given String, and fails when the input doesn't start with that.
-}
string :: String -> Parser String
string txt = case txt of
  (x:xs) -> mapP (:) (char x) `app` string xs
  "" -> mapP (const []) empty

{- The anyDigit parser matches Chars between 0 and 9.
-}
anyDigit :: Parser Char
anyDigit = filterP isDigit anyChar

{- The list combinator takes a parser and builds another parser that matches
any number (including zero) of repetitions of what the original one matched,
yielding a list.
-}
list :: Parser a -> Parser [a]
list p
  = (mapP (:) p `app` list p)
  `alt` (mapP (const []) empty)

{- The pair parser takes two parsers and builds another parser
that yields a pair of their results.
-}
pair :: Parser a -> Parser b -> Parser (a, b)
pair p q = mapP (,) p `app` q
