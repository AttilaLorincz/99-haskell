{-

-}

module ParserCombinators where

type Parser a = String -> [(a, String)]

runP :: Parser a -> String -> [a]
runP p s =
  [ x
  | (x, "") <- p s
  ]

empty :: a -> Parser a
empty x s = [(x, s)]

char :: Char -> Parser Char
char c s = case s of
  (x:xs) -> if x == c then [(x, xs)] else []
  [] -> []

anyChar :: Parser Char
anyChar s = case s of
  (x:xs) -> [(x, xs)]
  [] -> []

alt :: Parser a -> Parser a -> Parser a
alt p q s = p s ++ q s

pMap :: (a -> b) -> Parser a -> Parser b
pMap f p s = [(f x, r) | (x, r) <- p s]

pApp :: Parser (a -> b) -> Parser a -> Parser b
pApp p q s =
  [ (f x, r')
  | (f, r) <- p s
  , (x, r') <- q r
  ]

string :: String -> Parser String
string txt = case txt of
  (x:xs) -> pmap (:) (char x) `app` string xs
  "" -> empty ""

many :: Parser a -> Parser [a]
many p = (pmap (:) p `app` many p) `alt` empty []

pZip :: Parser a -> Parser b -> Parser (a, b)
pZip p q = pmap (,) p `app` q
