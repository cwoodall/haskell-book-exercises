module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = 
    case n of 
        0 -> "zero"
        1 -> "one"
        2 -> "two"
        3 -> "three"
        4 -> "four"
        5 -> "five"
        6 -> "six"
        7 -> "seven"
        8 -> "eight"
        9 -> "nine"
        _ -> error "Invalid digit"

digits :: Int -> [Int]
-- digits 0 = []
digits n = reverse $ (go n True)
        where
            go n isFirst
                | n == 0 && not isFirst = []
                | otherwise = digit : go rest False
                    where (rest, digit) = n `divMod` 10


wordNumber :: Int -> String
wordNumber n = joinList $ map digitToWord (digits n)
        where joinList = (concat . (intersperse "-"))

