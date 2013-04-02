-- Compile with the -O flag for efficiency and make sure to allocate enough space in the heap

import Data.Numbers.Primes
import Data.List

-- For a given quadratic of the form n^2 + a*n + b, find the 
-- length of the consecutive prime number chain
primeChain :: Integer -> Integer -> Int
primeChain a b = length $ takeWhile isPrime [n^2 + a*n + b | n <- [0..]]

-- Create a tuple max function (from PE 14)
tupMax a b
	| max (fst a) (fst b) == fst a = a
	| max (fst a) (fst b) == fst b = b

-- Print and write out the answer
main = do
		let primeChainTups = [(primeChain a b, a*b)| a <- [-1000.. 1000], b <- [-1000.. 1000]] 
		let ans = foldr1 tupMax primeChainTups
		writeFile "pe27.txt" $ show ans
		print ans