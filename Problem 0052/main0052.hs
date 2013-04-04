-- We are given that 125874 and 2*125874 are permutations of each other

import Data.Set
-- Define a function to check if n, 2n, 3n, 4n, 5n, and 6n are
-- permutations of each other
isPerm :: Integer -> Bool
isPerm n 
	| (head $ show n) /= '1' = False -- special case
	| originalNum /= (fromList $ show (2*n)) = False
	| originalNum /= (fromList $ show (3*n)) = False
	| originalNum /= (fromList $ show (4*n)) = False
	| originalNum /= (fromList $ show (5*n)) = False
	| originalNum /= (fromList $ show (6*n)) = False
	| otherwise = True
	where originalNum = fromList $ show n
	
-- Print and write out the answer
main = do
		let ans = head [n | n <- [125874..], isPerm n]
		writeFile "pe52.txt" $ show ans
		print ans