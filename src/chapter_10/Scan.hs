module Scan where

fibs :: [Int]
fibs = 1 : scanl (+) 1 fibs

fibsTill n = take n fibs

fibsLt100 = takeWhile (< 100) fibs

fact = scanl (*) 1 [1..] 
