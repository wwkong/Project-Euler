-- We are given that p=120 has 3 solutions

import Data.Set
import Data.List

-- Define a function that will return the number of solutions for a given parameter
numSolns p = size $ fromList [ sort [a,b,p-b-a] | a <- [1..p], b <- [1..p], a^2 + b^2 == (p-a-b) ^2 ]

-- Import our tuple max function from PE 14
tupMax a b
	| max (fst a) (fst b) == fst a = a
	| max (fst a) (fst b) == fst b = b

-- Print and write out the answer
main = do 
		let ans = foldr1 tupMax [ (numSolns n, n) | n <- [1..999] ]
		writeFile "pe39.txt" $ show ans
		print ans