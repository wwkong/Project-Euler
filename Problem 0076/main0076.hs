-- How many different ways can one hundred be written as a sum of at least two positive integers?

import qualified Data.MemoCombinators as Memo
import Data.Maybe
import Debug.Trace

-- This is pretty much the same as Problem #31 except that we no longer need our dictionary

-- Import our function to count the number of ways recursively (memoized)
-- numways' n k is the number of ways to get n with the first k types of numbers
numWays n = numWays' n (n-1) where
	numWays' n k
		-- | trace ("Called with n=" ++ show n ++ ", k=" ++ show k) False = undefined
		| n == 0 = 1
		| (k < 1) || (n < 0) = 0
		| otherwise = (numWays' n (k-1)) + (numWays' (n - k) k)
			-- The first expr. is the number of ways where we don't use the number k and the second
			-- is the number of ways in which do use it at least once
		
-- Print and write out the answer
main = do 
		let ans = numWays 100
		writeFile "pe76.txt" $ show ans
		print ans