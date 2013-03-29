-- Took 2.67s to compute.

import Numeric
import Data.Char

-- Define a function to check if a number is a palindrome in base 10 and 2
isPalin :: Integer -> Bool
isPalin n = reverse nDec == nDec &&
			reverse nBin == nBin
		where
			nDec = map digitToInt $ show n
			nBin = showIntAtBase 2 intToDigit n ""

-- Print and write out the answer
main = do
		let ans = sum [xs | xs <- [1.. (10^6-1)], isPalin xs == True]
		writeFile "pe36.txt" $ show ans
		print ans