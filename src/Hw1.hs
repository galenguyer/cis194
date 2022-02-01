module Hw1
    ( toDigits,
    toDigitsRev,
    doubleEveryOther,
    sumDigits
    ) where

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
    | n < 0         = []
    | n < 10        = [n]
    | otherwise     = toDigits (div n 10) ++ toDigits (mod n 10)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = (reverse . toDigits) n

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:[]) = (x * 2) : y : []
doubleEveryOther (x:y:zs)
    | mod (length zs) 2 == 0    = (x * 2) : y : (doubleEveryOther zs)  
    | otherwise                 = x : (y * 2) : (doubleEveryOther zs)

sumDigits :: [Integer] -> Integer
sumDigits = undefined