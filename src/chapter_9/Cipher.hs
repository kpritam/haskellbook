module Cipher where

import           Data.Char

shift0 :: Int -> Char -> Char -> Char -> Char
shift0 by start end c = chr $ adjust $ ord c + by
  where
    adjust n
      | n > ord end = n - ord end + ord start - 1
      | n < ord start = n + 26
      | otherwise = n

shiftBy :: Int -> Char -> Char
shiftBy n c
  | isLower c = shift0 n 'a' 'z' c
  | otherwise = shift0 n 'A' 'Z' c

ceaser :: String -> String
ceaser = map $ shiftBy 3

unceaser :: String -> String
unceaser = map $ shiftBy (-3)
