-- What is the smallest positive number that is evenly divisible
-- by all of the numbers from 1 to 20?

import Data.List

-- Print and write out the answer
main = do 
		let ans = foldr1 lcm [1..20]
		writeFile "pe5.txt" $ show ans
		print ans