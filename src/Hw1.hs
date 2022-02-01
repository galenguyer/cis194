module Hw1
    ( toDigits,
    toDigitsRev
    ) where

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n
    | n < 0 = []
    | n < 10 = [n]
    | otherwise = toDigits (div n 10) ++ toDigits (mod n 10)

toDigitsRev :: Int -> [Int]
toDigitsRev n = (reverse . toDigits) n