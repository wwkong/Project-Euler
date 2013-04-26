{-
Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits)
with the same digit, is part of an eight prime value family.

For example, by replacing the 1st digit of *3, it turns out that six of the nine possible 
values: 13, 23, 43, 53, 73, and 83, are all prime.

Also, by replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number 
is the first example having seven primes among the ten generated numbers, yielding the family:
56003, 56113, 56333, 56443, 56663, 56773, and 56993.
-}

import Data.Numbers.Primes
import Data.List
import Data.Char

-- Note that the number of repeated decimals must be equal to 3, since all other number of repeats
-- have a count of >2 in how many occurences of 0, 1, and 2 modulo 3 in their digit sum (of the repeated
-- digits). Hence, the total sum of digits will be divisible by 3 for at least 3 of the digit replaced numbers
-- and hence we won't get 8 primes

-- Create a function to return all prime numbers that have n digits
primesOfLen n = [ps | ps <- takeWhile (<10^(n+1)) primes, (length . show) ps == n]

-- Simulate substitution by first generating a binary mask of length n (with three 1s) to be zipped in the answer
-- Note that the last digit cannot be a 1 since primes are odd
binMask n = nub [ps | ps <- permutations $ (take (n-3) $ repeat False) ++ [True,True,True], last ps == False]

-- Define a function that takes a digit, a mask, and a number and zips it all together
zipMask digit mask num = read (zipWith (\m n -> if m then intToDigit digit else n) mask (show num)) :: Integer

-- Since 8 digits are used, the smallest repeating digit must be a 0, 1, or 2

-- Define a function, that when given a prime and a mask, determines how many prime values are in its family
-- and the first generated number
pValCount p m
	| not (isReplPrime 0 || isReplPrime 1 || isReplPrime 2) = (0,0)
	| otherwise = (head nums, length nums)
	where
		len = (length . show) p
		replP d = zipMask d m p
		isReplPrime d = (isPrime . replP) d
		nums = [replP d | d <- [0..9], (length . show) (replP d) == len, isReplPrime d]
		
-- Given a digit length n, create the list of primes that has a family length of at least m
pVals n m = [fst (pValCount ps ms) | ps <- primesOfLen n, ms <- binMask n, snd (pValCount ps ms) >= m] 
		
-- -- We now implement the solution and make an educated guess that the number of digits is at least 6

main = do
		let ans = (head . head) [pVals n 8 | n <- [6..]] 
		writeFile "pe51.txt" $ show ans
		print ans