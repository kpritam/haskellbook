module MyList where

myHead :: [a] -> Maybe a
myHead []    = Nothing
myHead (x:_) = Just x

myTail :: [a] -> Maybe [a]
myTail []     = Nothing
myTail (x:[]) = Nothing
myTail (_:xs) = Just xs

zip0 :: [a] -> [b] -> [(a, b)]
zip0 = zipWith0 (,)

zipWith0 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith0 _ [] _          = []
zipWith0 _ _ []          = []
zipWith0 f (x:xs) (y:ys) = f x y : zipWith0 f xs ys
