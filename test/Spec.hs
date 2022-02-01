import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Hw1

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [hw1]

hw1 :: TestTree
hw1 = testGroup "HW1" [hw1ToDigits, hw1ToDigitsRev, hw1DoubleEveryOther, hw1SumDigits]
    

hw1ToDigits :: TestTree
hw1ToDigits = testGroup "toDigits"
    [ testCase "1234" $
        toDigits 1234 @?= [1,2,3,4]
    , testCase "0" $
        toDigits 0 @?= []
    , testCase "(-17)" $
        toDigits (-17) @?= []
    ]

hw1ToDigitsRev :: TestTree
hw1ToDigitsRev = testGroup "toDigitsRev"
    [ testCase "1234" $
        toDigitsRev 1234 @?= [4,3,2,1]
    ]

hw1DoubleEveryOther :: TestTree
hw1DoubleEveryOther = testGroup "doubleEveryOther"
    [ testCase "[8,7,6,5]" $
        doubleEveryOther [8,7,6,5] @?= [16,7,12,5]
    , testCase "[1,2,3]" $
        doubleEveryOther [1,2,3] @?= [1,4,3]
    ]

hw1SumDigits :: TestTree
hw1SumDigits = testGroup "sumDigits"
    [ testCase "[16,7,12,5]" $
        sumDigits [16,7,12,5] @?= 22
    ]

-- properties :: TestTree
-- properties = testGroup "Properties" [scProps, qcProps]

-- scProps = testGroup "(checked by SmallCheck)"
--   [ SC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , SC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   , SC.testProperty "Fermat's last theorem" $
--       \x y z n ->
--         (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
--   ]

-- qcProps = testGroup "(checked by QuickCheck)"
--   [ QC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , QC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   , QC.testProperty "Fermat's last theorem" $
--       \x y z n ->
--         (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
--   ]

-- unitTests = testGroup "Unit tests"
--   [ testCase "List comparison (different length)" $
--       [1, 2, 3] `compare` [1,2] @?= GT

--   -- the following test does not hold
--   , testCase "List comparison (same length)" $
--       [1, 2, 3] `compare` [1,2,2] @?= LT
--   ]
