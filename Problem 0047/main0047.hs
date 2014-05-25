-- Find the first four consecutive integers to have four distinct prime factors.
-- What is the first of these numbers?

import           Data.Numbers.Primes
import           Data.List

-- Create a function to get the first four (or less) prime factors of a number
pFourFactors :: Int -> [Int]
pFourFactors n = (nub .primeFactors) n

-- Create a list to get the first number of 4 consecutive numbers that have
-- 4 distinct prime factors
fstFourDistinct :: [Int]
fstFourDistinct = [n |  n <- [2*3*5*7..],
                        let lenPF k = length $ pFourFactors k,
                        map lenPF [n.. (n+3)] == [4,4,4,4]]

-- Print and write out the answer
main :: IO()
main = do
        let ans = head fstFourDistinct
        writeFile "pe47.txt" $ show ans
        print ans
