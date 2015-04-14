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

pAlt :: Parser a -> Parser a -> Parser a
pAlt p q s = p s ++ q s

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
  (x:xs) -> pMap (:) (char x) `pApp` string xs
  "" -> empty ""

many :: Parser a -> Parser [a]
many p = (pMap (:) p `pApp` many p) `pAlt` empty []

pZip :: Parser a -> Parser b -> Parser (a, b)
pZip p q = pMap (,) p `pApp` q
