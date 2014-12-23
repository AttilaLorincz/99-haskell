module Logic where

import Data.Char (isSpace)

times :: String -> String
times x =
    let (word, rest) = splitWord x
    in nTimes (read word) rest

-- split a string into the first word and the rest
splitWord :: String -> (String, String)
splitWord str =
    let
      word = takeWhile (not . isSpace) str
      rest = drop (length word + 1) str
    in (word, rest)

-- repeat a string N times
nTimes :: Int -> String -> String
nTimes n s = undefined
