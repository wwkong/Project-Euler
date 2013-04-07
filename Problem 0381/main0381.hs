{-
For a prime p let S(p) = ((p-k)!) mod(p) for 1<=k<=5.

For example, if p=7,
(7-1)! + (7-2)! + (7-3)! + (7-4)! + (7-5)! = 6! + 5! + 4! + 3! + 2! = 720+120+24+6+2 = 872.
As 872 mod(7) = 4, S(7) = 4.

It can be verified that S(p) = 480 for 5<=p<=100.

Find S(p) for 5<=p<=108.
-}

-- Use the -O flag here when compiling as single threaded processing is WAY too slow

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