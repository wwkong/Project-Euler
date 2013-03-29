-- Took 6.75s to compute.

import Data.List

-- We use a little permutation hack here, sort, and return the millionth entry
main = do
		let nums = permutations "0123456789"
		let ans = (sort nums) !! (10^6-1)
		writeFile "pe24.txt" ans
		print (read ans :: Integer)