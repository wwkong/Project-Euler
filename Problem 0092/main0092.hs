{-
A number chain is created by continuously adding the square of the digits in a number
to form a new number until it has been seen before.

What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.
How many starting numbers below ten million will arrive at 89?
-}

import Data.Char
import Data.MemoCombinators as Memo

-- Create a function that checks whether or not a starting number will eventually cycle to 
-- 89 first
isEightyNine n
	| n == 89 = True
	| n == 1 = False
	| otherwise = isEightyNine next
	where 
		next = (sum . (map (^2)) . (map digitToInt) . show) n
		
-- Print and write out the answer		
main = do
		let ans = length [n | n <- [1..(10^7-1)], (Memo.integral isEightyNine) n]
		writeFile "pe92.txt" $ show ans
		print ans