{-
It can be seen that there are 3 fractions between 1/3 and 1/2.

How many fractions lie between 1/3 and 1/2 in the sorted set of reduced 
proper fractions for d <= 12,000?
-}

import Data.List
import Data.Ratio

-- Brute force approach

-- Produce all possible fractions
fracs = [a % b | b <- [1..12000], a <- [(b `div` 3 - 1)..(b `div` 2 + 1)], a % b < 1 % 2, a % b > 1 % 3, gcd a b == 1]

-- Print and write out the answer
main = do
		let ans = length fracs
		writeFile "pe73.txt" $ show ans
		print ans		