{-
Exactly four continued fractions, for N ≤ 13, have an odd period.
How many continued fractions for N ≤ 10000 have an odd period?
-}

import Data.Ratio
import Debug.Trace

-- Define a function which takes n,a0,b0 and returns a,b,c in a list with the input form a0/(sqrt(n)-b0) 
-- and output form a + (sqrt(n)-b)/c
iterNext :: Int -> Int -> Int -> [Int]
iterNext n a0 b0
	|(n - b0^2) == 0 = []
	| otherwise = [a,b,c] 
	where 
		a = (head [m | m <- [1..n],  m*c - b0 > fsN]) - 1
		b = -(b0 - a*c)
		c = denominator $ a0 % (n - b0^2)
		fsN = floor $ sqrt $ fromIntegral n
		
-- Define a function that takes in a number n and computes the cycle of the fraction representation of sqrt(n)
-- We implement the method in Corollary 3.3. of this paper: 
--	http://web.math.princeton.edu/mathlab/jr02fall/Periodicity/alexajp.pdf
fCycle n = fCycle' 1 b0 0 0
	where
		b0 =  floor $ sqrt $ fromIntegral n
		fCycle' a b tail iter
			| null next = 0
			| n == 2 = 1 -- Special case
			| 2*b0 == tail = iter 
			-- | trace (show nextA ++ " " ++ show nextB ++ " " ++ show nextC ++ " lst=" ++ show ws) False = undefined
			| otherwise = fCycle' nextC nextB nextA (iter+1) 
				where 
					next = iterNext n a b
					nextA = next !! 0
					nextB = next !! 1
					nextC = next !! 2					

-- Print and write out the answer
main = do
		let ans = length [xs | xs <- [1..10000], odd $ fCycle xs]
		writeFile "pe64.txt" $ show ans
		print ans