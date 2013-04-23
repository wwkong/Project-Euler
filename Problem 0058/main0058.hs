{-
Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.

37 36 35 34 33 32 31
38 17 16 15 14 13 30
39 18  5  4  3 12 29
40 19  6  1  2 11 28
41 20  7  8  9 10 27
42 21 22 23 24 25 26
43 44 45 46 47 48 49

It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting 
is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13  62%.

If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed. 
If this process is continued, what is the side length of the square spiral for which the ratio of primes along 
both diagonals first falls below 10%?
-}

import Data.MemoCombinators as Memo
import Data.Numbers.Primes
import Debug.Trace

-- We create a function to return a list of the four diagonals for a given length n
ulamCorners :: Integer -> [Integer]
ulamCorners n = [an, an - n + 1, an - 2*n + 2, an - 3*n + 3] where
	an = n ^ 2
	
-- Generate all spiral numbers and the number of primes for a block of size 2*n-1
ulams = [ulamCorners n | n <- [3,3+2..]]
primeUlams = map (length . (filter isPrime)) ulams

-- Accumulate until a limit is reached
findUlamLim limit lst =  findUlamLim' 1 0 limit lst where
	findUlamLim' ctr acc limit lst
		-- | trace ("ratio=" ++ show ratio) False = undefined
		| ctr == 1 = findUlamLim' (ctr + 2) (acc + (head lst)) limit (tail lst) -- Special case
		| ratio < limit = ctr
		| otherwise = findUlamLim' (ctr + 2) (acc + (head lst)) limit (tail lst)
		where ratio = (fromIntegral acc) / (fromIntegral ((ctr-1) * 2) + 1)

-- Print and write out the answer
main = do
		let ans = findUlamLim 0.1 primeUlams
		writeFile "pe58.txt" $ show ans
		print ans