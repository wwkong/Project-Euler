-- Find the sum of all the positive integers which cannot be written as the
-- sum of two abundant numbers.

-- By mathematical analysis, it can be shown that all integers greater than 
-- 28123 can be written as the sum of two abundant numbers. 

-- (More optimized version)
-- Inspired by http://en.wikipedia.org/wiki/Divisor_function and most of the credit is by
-- http://d.hatena.ne.jp/D_Rascal/20111230/1325253134

import           Data.Array
import           Data.List
import qualified Data.List.Stream    as S
import           Data.Numbers.Primes

-- Create a function to find the non distinct prime factors of n
mFactors :: Integer -> [Integer]
mFactors k = mFactors' k primes where
    mFactors' n ps@(ph:pt)
        | ph > n = []
        | n `mod` ph == 0 = ph : (mFactors' (n `div` ph) ps)
        | otherwise = mFactors' n pt

-- Define an efficient proper divisors sum function (using the divisor function in Wiki)
sumDivs :: Integer -> Integer
sumDivs k = -k + product (map f ps)
    where
    f (n, times) = sum [n ^ x | x <- [0..times]]
    ps = [(head n, length n) | n <- group $ mFactors k]

-- Use an array here since Haskell uses O(n) indexing for lists
abNums :: Array Integer Bool
abNums = array (1, 28123) [(x, sumDivs x > x) | x <- [1..28123]]

canBeWritten :: Integer -> Bool
canBeWritten n = or $ S.map (\x -> abNums ! x && abNums ! (n - x)) [1..n `div` 2]

-- Print and write out the answer
main :: IO()
main = do
        let ans = sum $ filter (not . canBeWritten) [1..28122]
        writeFile "pe23.txt" $ show ans
        print ans
