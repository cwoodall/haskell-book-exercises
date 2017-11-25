module DatabaseExample where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
                (fromGregorian 1911 5 1)
                (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbNumber 910
    , DbNumber 1012
    , DbDate (UTCTime
                (fromGregorian 1930 6 1)
                (secondsToDiffTime 34123))
    , DbString "Hello, world!"
    , DbDate (UTCTime
                (fromGregorian 1922 6 1)
                (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = [x | (DbDate x) <- db]

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = [x | (DbNumber x) <- db]

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber 

avgDb :: [DatabaseItem] -> Double
avgDb db = 
    if n == 0 then 0 else fromIntegral (sum numbers) / n
    where 
        numbers = filterDbNumber db
        n = fromIntegral $ length numbers