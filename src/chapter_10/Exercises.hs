module Exercises where

stops = "pbtdkg"

vowels = "aeiou"

stopVowelStop :: [(Char, Char, Char)]
stopVowelStop = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops]

stopVowelStopBeginWithA :: [(Char, Char, Char)]
stopVowelStopBeginWithA = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops, s1 == 'p']
