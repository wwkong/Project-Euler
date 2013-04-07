-- What is the largest prime factor of the number 600851475143 ?

import Data.Numbers.Primes

-- Define a function that returns the lowest prime factor
minPFactor :: Integer -> Integer
minPFactor n = head [xs | xs <- primes, n `mod` xs == 0]

-- Define the max prime factor function 
maxPFactor :: Integer -> Integer
maxPFactor n = maxPFactor' n 2 where
	maxPFactor' n p
		| n == 1 = p
		| otherwise = maxPFactor' (n `div` m) (max m p) where 
			m = minPFactor n
			
-- Print and write the answer
main = do 
		let ans = maxPFactor 600851475143
		writeFile "pe3.txt" $ show ans
		print ans