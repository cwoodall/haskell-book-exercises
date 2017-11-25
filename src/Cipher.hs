module Cipher where

import Data.Char

alphaShift :: Int -> Char -> Char
alphaShift x c
    | 'a' <= c && c <= 'z' = -- Shift and mod between 'a' and 'z'
        chr $ mod (ord (c) - ord 'a' + x) (26) + ord 'a' 
    | 'A' <= c && c <= 'Z' = -- shift the uppercase characters but maintain the casing
        (toUpper . alphaShift x . toLower) c
    | otherwise = c -- pass through nonshifted characters

caesar :: Int -> String -> String
caesar shift inStr = map (alphaShift shift) inStr         
  
unCaesar :: Int -> String -> String
unCaesar shift inStr = caesar (-shift) inStr