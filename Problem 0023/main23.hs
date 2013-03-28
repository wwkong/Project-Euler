-- Don't forget to compile with the -O flag

import qualified Data.Set as S

-- Import the naive integer sqrt function
squareRoot :: Integer -> Integer
squareRoot = floor . sqrt . (fromIntegral :: Integer -> Double)

-- Define an efficient proper divisors sum function
sumAliquot n = -n + sum [xs + (n `div` xs)| xs <- [1.. squareRoot n], n `mod` xs == 0, n `mod` xs /= xs]

-- Find all of the abundant numbers under 28123
abundantNums :: [Integer]
abundantNums = [xs| xs <- [1.. 28123], (sumAliquot xs) > xs]

-- Find all pairs of abundant numbers using the above list under 28123
diffLst :: [Integer]
diffLst = S.toList $ S.fromList $ [xs + ys | xs <- abundantNums, ys <- abundantNums, xs + ys <= 28123]

-- Print and write out the answer 

main = do 
		let ans = (sum [1.. 28123]) - (sum diffLst) 
		writeFile "pe23.txt" $ show ans
		print ans