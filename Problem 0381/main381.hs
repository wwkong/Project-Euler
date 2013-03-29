-- Use the -O flag here when compiling as single threaded processing is WAY too slow
-- Took 23.84s to compute.

import Data.List
import Data.Numbers.Primes

-- Implement an inverse generating function using the EEA
inverse n 1 = 1
inverse n p = (m * n + 1) `div` p
        where m = p - inverse p (n `mod` p)
		
-- We now apply a shortcut using Wilson's Theorem ( (p-1)! == -1 mod p iff p is prime )
-- It can be shown that S(p) = (p-5)!*9 mod p = -3/8 mod p

-- Print and write out the answer
main = do 
		let ans = sum [((-3) * (inverse xs 8)) `mod` xs | xs <- takeWhile (<10^8) (filter (>=5) primes)]
		writeFile "pe381.txt" $ show ans
		print ans		