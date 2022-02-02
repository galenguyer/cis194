module Hw1
    ( toDigits,
    toDigitsRev,
    doubleEveryOther,
    sumDigits,
    validate,
    Move,
    Peg,
    hanoi
    ) where

toDigits :: Integer -> [Integer]
toDigits n
    | n < 1         = []
    | n < 10        = [n]
    | otherwise     = digits n

digits :: Integer -> [Integer]
digits n
    | n < 0         = []
    | n < 10        = [n]
    | otherwise     = digits (div n 10) ++ digits (mod n 10)

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
sumDigits [] = 0
sumDigits (x:[])
    | x < 10    = x
    | otherwise = sumDigits [(div x 10)] + (mod x 10)
sumDigits (x:ys) = (sumDigits [x]) + sumDigits ys

validate :: Integer -> Bool
validate n = do
    (mod ((sumDigits . doubleEveryOther . toDigits) n) 10) == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n source dest aux
    | n == 1        = [(source, dest)]
    | otherwise = do
        hanoi (n - 1) source aux dest ++ [(source, dest)] ++ hanoi (n - 1) aux dest source
