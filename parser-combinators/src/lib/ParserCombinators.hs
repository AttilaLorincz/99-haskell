module ParserCombinators where

{- A Parser t is a function that takes input of type String and yields a pair,
containing some result of type t and the leftover of type String. To be able to
model ambiguity, it actually yields a list of such pairs. The empty list
represents failure.
-}
type Parser t = String -> [(t, String)]

{- We call a parse successful when the leftover String is empty.
The runP takes a parser and an input String, and returns the
list of results from successful parses.
-}
runP :: Parser a -> String -> [a]
runP p s =
  [ x
  | (x, "") <- p s
  ]

-- PARSERS --

{- The empty parser matches the empty String "" and yields ().
It always succeeds, consuming no input.
-}
empty :: Parser ()
empty s = [((), s)]

{- The char function constructs a parser that matches (and yields)
the given Char, and fails when the input doesn't start with that.
-}
char :: Char -> Parser Char
char c s = case s of
  (x:xs) -> if x == c then [(x, xs)] else []
  [] -> []

{- The anyChar parser matches (and yields) any Char.
It can only fail when the input String is empty.
-}
anyChar :: Parser Char
anyChar s = case s of
  (x:xs) -> [(x, xs)]
  [] -> []

{- The string function constructs a parser that matches (and yields)
the given String, and fails when the input doesn't start with that.
HINT: This parser can be built from the ones above with the combinators below.
-}
string :: String -> Parser String
string txt = case txt of
  (x:xs) -> mapP (:) (char x) `app` string xs
  "" -> mapP (const []) empty

-- COMBINATORS --

{- Parser combinators are functions that build new parsers from existing ones.
They are higher-order functions, because parsers are functions themselves.
-}

{- The mapP combinator takes a function f and a parser p,
and builds a parser which yields the results of p sent through f.
-}
mapP :: (a -> b) -> Parser a -> Parser b
mapP f p s = [(f x, r) | (x, r) <- p s]

{- The alt combinator takes two parsers, and builds a parser
that yields a result whenever any of the two parsers does.
-}
alt :: Parser a -> Parser a -> Parser a
alt p q s = p s ++ q s

{- The app combinator takes a parser that yields a function and
a parser that yields a value, and builds a new parser that yields
the result of the function applied to the value.
-}
app :: Parser (a -> b) -> Parser a -> Parser b
app p q s =
  [ (f x, r')
  | (f, r) <- p s
  , (x, r') <- q r
  ]

{- The list combinator takes a parser and builds another parser that matches
any number (including zero) of repetitions of what the original one matched,
yielding a list.
-}
list :: Parser a -> Parser [a]
list p = (mapP (:) p `app` list p) `alt` (mapP (const []) empty)

{- The pair parser takes two parsers and builds another parser that yields
the results of running both of them in sequence as a pair.
-}
pair :: Parser a -> Parser b -> Parser (a, b)
pair p q = mapP (,) p `app` q
