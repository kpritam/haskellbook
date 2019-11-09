module Exercises where

import           Chapter9.PoemLines
import           Data.Char

capitalizeWords :: String -> [(String, String)]
capitalizeWords [] = []
capitalizeWords s = (word, capitalized word) : map (\w -> (w, capitalized w)) rest
  where
    word:rest = split ' ' s
    capitalized []          = []
    capitalized (head:tail) = toUpper head : map toLower tail
