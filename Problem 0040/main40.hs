-- Took 0.42s to compute

import Data.List
import Data.Char

-- Create a function to find the n-th digit of Champernowne's constant
champ :: Int -> Int
champ n = digitToInt $ champStr !! n where 
	champStr = concatMap show [n | n <- [0..]]
	
-- Print and write out the answer
main = do
		let ans = 
				champ 1 * champ 10 * champ (10^2) *
				champ (10^3) * champ (10^4) *
				champ (10^5) * champ (10^6)
		writeFile "pe40.txt" $ show ans
		print ans