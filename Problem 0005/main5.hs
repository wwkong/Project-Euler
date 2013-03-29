-- Took 0.00s to compute.

import Data.List

-- Print and write out the answer
main = do 
		let ans = foldr1 lcm [1..20]
		writeFile "pe5.txt" $ show ans
		print ans