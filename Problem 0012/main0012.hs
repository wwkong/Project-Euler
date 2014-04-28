-- What is the value of the first triangle number to have
-- over five hundred divisors?

import           Data.List
import           Data.Numbers.Primes

-- Create a function to calculate the number of divisors for a number
nDivisors n = product $ map ((+1) . length) (group (primeFactors n))

-- Print and write out the answer
main = do
        let ans = head $ filter (>500) $ map nDivisors $ scanl1 (+) [1..]
        writeFile "pe12.txt" $ show ans
        print ans
