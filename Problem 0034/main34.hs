-- We first note that the upper bound for this problem is 999999 (six nines)
-- Took 3.02s to compute.

import Data.List
import Data.Char

-- Define our limit
limit = 999999

-- Memoization here is key; we first define an efficient factorial function
factrl :: Int -> Integer
factrl = (map factrl' [0..] !!) where
	factrl' 0 = 1
	factrl' n = (toInteger n) * factrl (n-1)
	
-- Define a function to check if a number is 'curious' ;D
isCurious :: Integer -> Bool
isCurious n = (== n) $ sum $ map factrl $ map digitToInt $ show n 

-- Print and write out the answer
main = do
		let ans = sum $ filter isCurious [3..limit]
		writeFile "pe24.txt" $ show ans
		print ans