module DbFolds where

import           Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr getTimes []
  where
    getTimes (DbDate time) acc = time : acc
    numbers _ acc = acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr getNumbers []
  where
    getNumbers (DbNumber n) acc = n : acc
    getNumbers _ acc            = acc

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sumDb db) / totalNumbers
  where
    totalNumbers = (fromIntegral . length . filterDbNumber) db
