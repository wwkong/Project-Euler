-- What is the largest n-digit pandigital prime that exists?

{-
First note that an n digit pandigital prime (assuming 1<=n<=9) is at most 7
digits since the sum of the digits for n=8 and n=9 is divisible by 3.
Thus, we'll do a brute force approach using 7 digit permutations
-}

import           Data.List
import           Data.Numbers.Primes

-- Create all possible 7 digit pandigital numbers in descending order
pdNums = (reverse . sort) $ (map (read :: String -> Integer)) $ permutations "1234567"

-- Print and write out the answer
main = do
        let ans = head [ n | n <- pdNums, isPrime n ]
        writeFile "pe41.txt" $ show ans
        print ans
