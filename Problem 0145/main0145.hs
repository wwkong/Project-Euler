{-
Some positive integers n have the property that the sum [n + reverse(n)] consists entirely of odd (decimal) digits. For instance, 36 + 63 = 99 and 409 + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and 904 are reversible. Leading zeroes are not allowed in either n or reverse(n).

There are 120 reversible numbers below one-thousand.

How many reversible numbers are there below one-billion (10^9)?
-}

import Data.Char
import Data.MemoCombinators as Memo

-- Get the set of odd digit numbers of length n
odds :: [Int]
odds = [1,3,5,7,9]
oddDigitNums :: Int -> [[Int]]
oddDigitNums 1 = map (:[]) odds
oddDigitNums n = oddDigitNums (n-1) >>= (\d -> [k:d | k <- odds])

-- Get the complete set of odd digit numbers up to length n
allOdds :: Int -> [[Int]]
allOdds = Memo.integral allOdds' where
    allOdds' 1 = oddDigitNums 1
    allOdds' n = oddDigitNums n ++ allOdds' (n-1)

-- For a given integer representation a~=[a1,a2,...,an], return the number unique numbers b so that b+rev(b)=a
numRevPairs :: [Int] -> Int
numRevPairs as = length predLst where
    predLst = [ b | b <- [minRng..maxRng], b + revN b == n]
    n = read (map intToDigit as)
    minRng = 10 ^ (length as-2) + 1
    maxRng = n `div` 2
    revN = read . reverse . show

-- Try out the test example
test :: Int
test = 1

-- Print and write
main :: IO()
main = do
        let ans = length $ allOdds 9
        writeFile "pe145.txt" $ show ans
        print ans
