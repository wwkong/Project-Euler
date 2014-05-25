-- What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

import           Data.Numbers.Primes

-- Import the naive square root function
squareRoot :: Integer -> Integer
squareRoot = floor . sqrt . (fromIntegral :: Integer -> Double)

-- Write a function to check if a number satisfies Goldbach's weak conjecture
weakGoldbach :: Integer -> Bool
weakGoldbach k = weakGoldbach' k primes where
    weakGoldbach' n lst
        | (head lst) > n = False
        | (squareRoot pSquare) ^ 2 == pSquare = True
        | otherwise = weakGoldbach' n (tail lst)
        where pSquare = (n - (head lst)) `div` 2

-- Print and write out the answer
main :: IO()
main = do
        let ans = head [n | n <- [33..], odd n, not $ weakGoldbach n]
        writeFile "pe46.txt" $ show ans
        print ans
