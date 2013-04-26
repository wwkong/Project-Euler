{-
Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0,
where each “_” is a single digit.
-}

import Control.Monad

-- Import the naive integer square root function
intSqrt = floor . sqrt . fromIntegral

-- Define our upper and lower bounds
minB = (intSqrt 1020304050607080900) `div` 10 
maxB = (intSqrt 1929394959697989990) `div` 10

-- Define a function to check if a number is a perfect square
isPSquare n = (intSqrt n) ^ 2 == n

-- Get the significant digits of a number string (every even index) as a string
sigDigits nStr = [nStr !! k | k <- [0,2..16]]

-- Print and write out the ans
main = do
		let ans = 10 * (head [ n | n <- [minB..maxB], sigDigits (show (n^2)) == "123456789"])
		writeFile "pe206.txt" $ show ans
		print ans