{-
The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes 
and concatenating them in any order the result will always be prime. For example, 
taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, 
represents the lowest sum for a set of four primes with this property.

Find the lowest sum for a set of five primes for which any two primes concatenate 
to produce another prime.
-}

import Data.Numbers.Primes
import Data.List
import Control.Applicative
import Debug.Trace

-- Define an upper bound (subject to change)
upBound = 10000

-- Create a function to determine is two primes can be concatenated in any order
-- to produce another prime
concatPrime :: Integer -> Integer -> Bool
concatPrime n m = let rStr = read :: String -> Integer in 
	(isPrime . rStr) (show n ++ show m) && (isPrime . rStr) (show m ++ show n)
	
-- Given a list, find a list of primes that produce the next group of
-- concatenable primes using the max of the list as a lower bound and 
-- upBound as the upper bound
nextCPrimes :: [Integer] -> [Integer]
nextCPrimes cLst = let biggest = maximum cLst in
	[next | next <- filter (> biggest) (takeWhile (<upBound) primes), all (==True) ([concatPrime next] <*> cLst)]		
		
-- Define a function that takes in a generating number and group length and 
-- returns all groups that are composed of concatenable primes
genCPrimes n len = filter (/=[]) [genCPrimes' n len [k] [n] | k <- nextCPrimes [n]] where
	genCPrimes' n' len' lst' ret'-- lst' is a list of candidates
		-- | trace ("\ngenCPrimes' " ++ show n' ++ " " ++ show len' ++ " ") False = undefined
		| len' == 1 = [n']
		| null lst' = []
		| len' > 1 + (length nextIter) = genCPrimes' n' len' (tail lst') ret'
		-- | trace ("\ngenCPrimes' otherwise COND") False = undefined
		| otherwise = [n'] ++ nextIter
		where 
			nextLst = nextCPrimes ([head lst'] ++ ret')
			nextElem = head lst'
			nextIter = genCPrimes' nextElem (len'-1) nextLst (ret' ++ [nextElem])
			
main = do
		let ans = minimum $ ((map sum) . concat) [genCPrimes n 5 | n <- takeWhile (<100) primes] -- Just some guessing
		writeFile "pe60.txt" $ show ans
		print ans