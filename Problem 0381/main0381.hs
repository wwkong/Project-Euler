{-
For a prime p let S(p) = ((p-k)!) mod(p) for 1<=k<=5.

For example, if p=7,
(7-1)! + (7-2)! + (7-3)! + (7-4)! + (7-5)! = 6! + 5! + 4! + 3! + 2! = 720+120+24+6+2 = 872.
As 872 mod(7) = 4, S(7) = 4.

It can be verified that S(p) = 480 for 5<=p<=100.

Find S(p) for 5<=p<=10^8.
-}

-- Use the -O flag here when compiling as single threaded processing is WAY too slow

import qualified Data.List.Stream as S
import qualified Math.NumberTheory.Primes as P

-- We now apply a shortcut using Wilson's Theorem ( (p-1)! == -1 mod p iff p is prime )
-- It can be shown that S(p) = (p-5)!*9 mod p = -3/8 mod p

-- The following function is based off the one found in the PE 381 forum, given by 
-- Lucy_Hedgehog
s :: Integer -> Integer 
s p = ((3*p `mod` 8)*p - 3) `div` 8

-- Print and write out the answer
main :: IO()
main = do
		let nums = map s $ S.takeWhile (<10^8) $ S.dropWhile (<5) P.primes
		let ans = sum nums
		writeFile "pe381.txt" $ show ans
		print ans
