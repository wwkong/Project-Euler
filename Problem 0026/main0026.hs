-- Find the value of d<1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

-- This is my first attempt at cutting back on the amount
-- of static functions in my solutions

import Data.List

-- Create a function that converts a fraction of the 
-- form 1/b to its decimal expansion in list form
toDecLst b = tail $ toDecLst' 1 b where
	toDecLst' a b
		| a == b = [1] ++ (repeat 0)
		| a < b = 0 : toDecLst' (a*10) b 
		| a `mod` b == 0 = [a `div` b] ++ (repeat 0)
		| a > b = (a `div` b) : toDecLst' (a `mod` b * 10) b 
		
-- Note that the maximum cycle length of fraction 1/b is always b-1	which follows from
-- Euler's theorem for a = 10, n = b, since the Euler-Totient function of n is always 
-- less than n-1

-- We create a function to compute the largest cycle of n with this information
lCycle :: Int -> Int
lCycle 1 = 0 -- Special case
lCycle n = lCycle' 1 (toDecLst n) where
	lCycle' ptr nDec
		| ptr  > (n-1) = (-n+1) + lCycle' 1 (tail nDec)
		| take (n-1) nDec == take (n-1) ptrDec = 1
		| otherwise = 1 + (lCycle' (ptr + 1) nDec)
		where ptrDec = iterate tail nDec !! ptr

-- Create a tuple max function
tupMax a b
	| max (fst a) (fst b) == fst a = a
	| max (fst a) (fst b) == fst b = b

-- Exclude all multiples of 2 and 5 for efficiency (they aren't co-prime to 10)
cNums = [n | n <- [1..1000], n `mod` 5/= 0, not (even n)]

-- Print and write out the answer
main = do 
		let ans = foldl1 tupMax [(lCycle n, n) | n <- cNums]
		writeFile "pe26.txt" $ show ans
		print ans