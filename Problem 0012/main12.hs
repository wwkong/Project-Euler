-- Took 1.02s to compute.

import Data.Numbers.Primes
import Data.List

-- Create a function to calculate the number of divisors for a number
nDivisors n = product $ map ((+1) . length) (group (primeFactors n))

-- Print and write out the answer
main = do 
		let ans = head $ filter (>500) $ map nDivisors $ scanl1 (+) [1..]
		writeFile "pe12.txt" $ show ans
		print ans