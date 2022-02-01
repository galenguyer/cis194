module Hw1
    ( toDigits,
    toDigitsRev,
    doubleEveryOther
    ) where

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n
    | n < 0         = []
    | n < 10        = [n]
    | otherwise     = toDigits (div n 10) ++ toDigits (mod n 10)

toDigitsRev :: Int -> [Int]
toDigitsRev n = (reverse . toDigits) n

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:[]) = (x * 2) : y : []
doubleEveryOther (x:y:zs)
    | mod (length zs) 2 == 0    = (x * 2) : y : (doubleEveryOther zs)  
    | otherwise                 = x : (y * 2) : (doubleEveryOther zs)
