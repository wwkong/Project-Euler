{-
It is possible to write ten as the sum of primes in exactly five different ways:

7 + 3
5 + 5
5 + 3 + 2
3 + 3 + 2 + 2
2 + 2 + 2 + 2 + 2

What is the first value which can be written as the sum of primes in over five thousand different ways?
-}

import           Data.List
import           Data.MemoCombinators as Memo
import           Data.Numbers.Primes

--Modify the relevant function from PE 31:

-- Create a function to count the number of ways recursively (memoized)
-- numways' n is the number of ways to get n with the primes less than n
numWays :: Int -> Int
numWays n = numWays' n primeLst where
    primeLst = takeWhile (<n) primes
    numWays' m pLst
        | m == 0 = 1
        | m <  0 || null pLst = 0
        | otherwise = numWays' m (tail pLst) + numWays' (m-p) pLst
            -- The first expr. is the number of ways where we don't use the k-th coin and the second
            -- is the number of ways in which do use it at least once
        where p = head pLst

-- Search all numbers from a starting point while memoizing past occurences
searchWays :: Int -> Int
searchWays n0 = 1

-- Print and write
main :: IO()
main = do
        let ans = head [x | x <- [1..], numWays x > 5000]
        writeFile "pe77.txt" $ show ans
        print ans
