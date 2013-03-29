-- Took 0.00s to compute.

import Data.Char

-- Print and write the 
main = do
		let ans = sum $ map digitToInt $ show (2 ^ 1000)
		writeFile "pe16.txt" $ show ans
		print ans