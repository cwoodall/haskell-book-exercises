module Main where

import Lib
import WordNumber
import Data.Char

main :: IO ()
main = someFunc

triple :: Int -> Int
triple x = 3 * x

foo x = 
    let y = x * 2
        z = x ^ 2
    in 2 * y * z

doSomePrinting :: IO ()
doSomePrinting = do
    putStrLn "Wow this is a lot"
    putStrLn "of printing"

data Mood = Woot | Blah deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib x =  fib (x - 2) + fib (x - 1)

-- This is pretty sweet actually
printTupIfOne :: (Int, [Char]) -> IO()
printTupIfOne (1, x) = putStrLn x
printTupIfOne x = putStrLn "Nope"

myAbs :: Integer -> Integer
myAbs x = 
    if x >= 0
        then x
    else (-1) * x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
-- f x y = ((snd x, snd y), (fst x, fst y))
-- can also be expressed with pattern matching as
f (a, b) (c, d) = ((b, d), (a, c))
-- In my opinion this is a lot more readbale.

-- # Chapter 5: Types
-- 
-- - Haskell's type sytem is related to System F (may be interesting to look into this).
--   adds on the Hindley-Miller system for type inference.
-- - Types exist to impose constraints and enforce correctness They also:
--   - Eliminate certain classes of bugs and need for certain types of tests
--   - Allow for advanced compiler optimizations since intent is clearer
--   - Self-Documentation 
--  The big arrow operator (=>) is the type class operator this occurs to define what
--  type class generic types must implement. For example
mustBeNums :: (Num a) => a -> a
mustBeNums x = x
-- In this case any number type will be accepted, but otherwise they will be rejected.
-- This means that type a MUST belong to the Num typeclass.
--
-- ## 5.4 Currying
-- The definition of (->) as:
--    data (->) a b
-- means that currying is the default. All functions are techincally nested.
-- For example a function f with type `f :: a -> (b -> c)` would be a function f that 
-- takes an argument of type a and returns a function which types a type b and returns 
-- a function that returns type c 

-- Chapter 6: Typeclasses
-- A Typeclass is similar to an interface but applied to various types after the fact 
-- rather than a specific parent of a class.
data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun 
    deriving (Eq, Ord, Show, Enum)
data Date = Date DayOfWeek Int deriving Show

instance Eq Date where  
    (==) (Date weekday date)
         (Date weekday' date') =
            weekday == weekday' && date == date'

testEither :: Either Bool Int -> IO ()
testEither (Right a) = print "Int"
testEither (Left b) = print "Bool"

data FP88 = FP88 (Integer, Integer) deriving Show

instance Eq FP88 where
    (==) (FP88 (a,  b))
         (FP88 (a', b')) = 
            a == a' && b == b'

instance Num FP88 where
    (+) (FP88 (a,  b))
         (FP88 (a', b')) =
            FP88 (a'' , b'' `mod` 256)
            where b'' = b + b'
                  a'' = a + a' + (b'' `div` 256)
    (*) (FP88 (a,  b))
        (FP88 (a', b')) =             -- TODO (cw) incorrect, fix        
            FP88 ((c `div` 65536) `mod` 256, (c `mod` 65536) `div` 256)
            where c = ((a*256+b) * (a'*256+b'))
    (abs) (FP88 (a,  b)) =
        FP88 (abs a, abs b)

    (signum) (FP88 (a,  b)) =
        FP88 (signum a, 0)
    (negate) (FP88 (a,  b)) =
        FP88 ((-a), abs b)
    (fromInteger) x =
        FP88 (fromInteger x, 0)

testAdd :: Num a => a -> a -> a
testAdd x y = x + y

weirdAdd :: (Num a, Ord a) => a -> a -> a
weirdAdd x y = 
    if x > 1
        then x+y
    else x

-- Ch 6 Type-Kwond-Do 2:
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = 
    (f(x)) == y

arith :: Num b
    => (a -> b)
    -> Integer
    -> a
    -> b
arith f x y = f y + fromInteger x

-- ## CHAPTER 7: More Functional Patterns

-- is a palindrome

pal xs = 
    case xs == reverse xs of 
        True -> "yes"
        _    -> "no"
    where y = xs == reverse xs

fibGuard :: Integer -> Integer
fibGuard x 
       | x == 0 = 1
       | x == 1 = 1
       | otherwise = fibGuard (x - 1) + fibGuard (x-2)

type Id = Integer
type Len = Integer
type Data = [Integer]
data CanFrame = CanFrame Id Len Data deriving Show

filterCan :: CanFrame -> Maybe CanFrame
filterCan (CanFrame id len x)
        | id == 100 = Just $ CanFrame id len x
        | otherwise = Nothing

type PosSlope = Double
type NegSlope = Double
data Ramp = Ramp PosSlope NegSlope

applyRampLimits :: Ramp -> Double -> Double -> Double ->  Double
applyRampLimits (Ramp pos_lim neg_lim) next cur dt
        | deriv > pos_lim = cur + pos_lim * dt
        | deriv < neg_lim = cur - neg_lim * dt
        | otherwise       = next
        where deriv = (next - cur) / dt 

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
    where (xLast, _) = divMod x 100
          (_, d) = divMod xLast 10

-- Chapter 8: Recursion

-- The classic factorial
simpleFact :: Integer -> Integer
simpleFact 0 = 1
simpleFact x = x * simpleFact (x-1)

-- Division example
type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
        where go n   d count
               | n < d = (count, n)
               | otherwise = 
                    go (n - d) d (count + 1)

mc91 :: (Integral a) => a -> a
mc91 x
    | x > 100 = x - 10
    | otherwise = 91

-- see src/WordNumber.hs for more examples from this chapter

-- Chapter 9: Lists

-- list head operators
myHead :: [a] -> a
myHead (x : _) = x

myTail :: [a] -> [a]
myTail [] = [] -- this case causes an exception otherwise
myTail (_ : xs) = xs

-- or we can handle the [] case of myTail with the Maybe type
maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing       -- Return nothing instead of []
maybeTail (x:[]) = Nothing   -- For consistency
maybeTail (_:xs) = Just xs   -- Just return the array (Some in Rust)

-- Excercises: Thy Fearful Symmetjry
mySplit :: Char -> String -> [String]
mySplit _ "" = []
mySplit c s = 
    splitFront s : splitTail s
    where splitFront = takeWhile (/= c)
          splitTail = mySplit c . drop 1 . dropWhile (/= c)

-- Chapter 9.12: Chapter Excercises

capitalizeFirst :: String -> String
capitalizeFirst "" = ""
capitalizeFirst (c : s) = (toUpper c) : s

capitalizeAll :: String -> String
capitalizeAll "" = ""
capitalizeAll s = (toUpper (head s)) : capitalizeAll (tail s)

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain a = squishMap (\x -> x) a

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x:[]) = x
myMaximumBy cmp (x:xs) = 
    let 
        y = myMaximumBy cmp xs
    in 
        case cmp x y of 
            LT -> y
            GT -> x
            EQ -> y
            
-- Chapter 10 examples
stops = "pbtdkg"
vowels = "aeiou"

stopVowelStop :: String -> String -> [(Char, Char, Char)]
stopVowelStop stops vowels =
    [(s,v,s') | s <- stops
              , v <- vowels
              , s' <- stops
              , s == 'p'
              ]
        