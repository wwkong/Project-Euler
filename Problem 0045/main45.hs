{-
It can be shown with some arithmetic and manipulation of the quadratic equation, that a number N is Pentagonal
if 1+24*N is a perfect square and (sqrt(1+24*N) + 1) `mod` 6 == 0. Similarly, N is Hexagonal if 1+8*N is a perfect 
square and (sqrt(1+8*N) + 1) `mod` 4 == 0. We also know from a previous problem that N is a triangle number when 
1+8*N is a perfect square and (sqrt(1+8*N) + 1) `mod` 2 == 0.
-}

import Data.List
		
-- Import the naive square root function
squareRoot :: Integer -> Integer
squareRoot = floor . sqrt . (fromIntegral :: Integer -> Double)

-- Define a function to check if a word is a triangle number, pentagonal number, and hexagonal number
isTPHNum :: Integer -> Bool
isTPHNum n
	|	discrim1 ^ 2 == 1+8*n && 
		discrim2 ^ 2 == 1+24*n && 
		(discrim1 + 1) `mod` 4 == 0 &&
		(discrim2 + 1) `mod` 6 == 0 = True
	| 	otherwise = False
	where 
		discrim1 = squareRoot (1+8*n)
		discrim2 = squareRoot (1+24*n)

-- Start a new hexagonal number list for efficiency
newHNums = [n*(2*n-1) | n <- [144..]]
		
-- Print and write out the answer
main = do
		let ans = head [n | n <- newHNums, isTPHNum n]
		writeFile "pe45.txt" $ show ans
		print ans