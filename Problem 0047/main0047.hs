-- Find the first four consecutive integers to have four distinct prime factors.
-- What is the first of these numbers?

import           Data.Numbers.Primes

-- Create a function to get the first four (or less) prime factors of a number
pFourFactors n = take 4 [ps | ps <- takeWhile (<n) primes, n `mod` ps == 0]

-- Create a list to get the first number of 4 consecutive numbers that have
-- 4 distinct prime factors
fstFourDistinct = [n |  n <- [644..],
                        let lenPF n = length $ pFourFactors n,
                        map lenPF [n.. (n+3)] == [4,4,4,4]]

-- Print and write out the answer
main = do
        let ans = head fstFourDistinct
        writeFile "pe47.txt" $ show ans
        print ans
