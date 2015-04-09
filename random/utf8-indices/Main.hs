{-# LANGUAGE LambdaCase #-}
import Codec.Binary.UTF8.String
import qualified Data.IntMap.Strict as Map
import Data.List
import Data.Maybe
import Data.Word

type LinPart = (Int, Int)

type IndexMap = Map.IntMap LinPart

runIndexMap :: IndexMap -> Int -> Int
runIndexMap m i =
  let
    (domx, rngx, increment) = case Map.lookupLE i m of
      Nothing -> (0, 0, 1)
      Just (domx, (rngx, increment)) -> (domx, rngx, increment)
  in rngx + (i - domx) `div` increment

indexMapFromUtf8 :: [Word8] -> IndexMap
indexMapFromUtf8
  = Map.fromList
  . linPartsFromUtf8Chunks
  . chunkUtf8Seqs
  . separateUtf8Seqs

-- precondition: UTF-8 sequences in a chunk have the same length
linPartsFromUtf8Chunks :: [[Word8]] -> [(Int, LinPart)]
linPartsFromUtf8Chunks = linPartsFromUtf8Chunks' 0 0

linPartsFromUtf8Chunks' :: Int -> Int -> [[Word8]] -> [(Int, LinPart)]
linPartsFromUtf8Chunks' domx rngx = \case
  (chunk:chunks) ->
    let
      len = length chunk
      increment = utf8SeqLen . head $ chunk
      domx' = domx + len
      rngx' = rngx + len `div` increment
    in (domx, (rngx, increment)) : linPartsFromUtf8Chunks' domx' rngx' chunks
  [] -> []

-- merges neighboring UTF-8 sequences that have the same length
chunkUtf8Seqs :: [[Word8]] -> [[Word8]]
chunkUtf8Seqs
  = map (\chunks -> concatMap snd chunks)
  . groupBy (\x y -> fst x == fst y)
  . map (\chunk -> (utf8SeqLen . head $ chunk, chunk))

-- separates UTF-8 sequences
separateUtf8Seqs :: [Word8] -> [[Word8]]
separateUtf8Seqs = \case
  (bytes@(c:cs)) ->
    let
      len = utf8SeqLen c
      (bite, rest) = splitAt len bytes
    in bite : separateUtf8Seqs rest
  [] -> []

-- determines the length of the UTF-8 sequence based on its first byte
utf8SeqLen :: Word8 -> Int
utf8SeqLen c
  | c <= 0x7F = 1
  | c <= 0xDF = 2
  | c <= 0xEF = 3
  | otherwise = 4
