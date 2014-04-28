-- Find the sum of all the positive integers which cannot be written as the
-- sum of two abundant numbers.

-- More optimized version
-- Inspired by http://en.wikipedia.org/wiki/Divisor_function and most of the credit is by
-- http://d.hatena.ne.jp/D_Rascal/20111230/1325253134
-- Don't forget to compile with the -O flag

import           Data.Array
import           Data.List
import           Data.Numbers.Primes

-- Create a function to find the non distinct prime factors of n
mFactors :: Integer -> [Integer]
mFactors n = mFactors' n primes where
    mFactors' n ps@(ph:pt)
        | ph > n = []
        | n `mod` ph == 0 = ph : (mFactors' (n `div` ph) ps)
        | otherwise = mFactors' n pt

-- Define an efficient proper divisors sum function (using the divisor function in Wiki)
sumAliquot :: Integer -> Integer
sumAliquot n = -n + product (map f ps)
    where
    f (n, times) = sum [n ^ x | x <- [0..times]]
    ps = [(head n, length n) | n <- group $ mFactors n]

-- Use an array here since Haskell uses O(n) indexing for lists
canBeWritten :: Integer -> Bool
canBeWritten n = or $ [1..n `div` 2] >>= \x -> [abInds ! x && abInds ! (n - x)]
    where abInds = array (1, 28123) [(x, sumAliquot x > x) | x <- [1..28123]]

-- Print and write out the answer
main = do
        let ans = sum $ filter (not . canBeWritten) [1..28122]
        writeFile "pe23.txt" $ show ans
        print ans
