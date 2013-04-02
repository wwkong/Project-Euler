-- Make sure to compile via GHC using the -O flag and to allocate enough memory in the heap!

import Data.List

-- Define the collatz function which returns the length of a path
mCollatz n = mCollatz' n 1 where
	mCollatz' n m
		| n == 1 = m
		| even n = mCollatz' (n `div` 2) (m + 1)
		| odd n = mCollatz' (3*n + 1) (m + 1)

-- Create a tuple max function
tupMax a b
	| max (fst a) (fst b) == fst a = a
	| max (fst a) (fst b) == fst b = b
		
-- Print and write out the answer
main = do
		let mColLens = map mCollatz [1..(10^6-1)]
		let mCols = zip mColLens [1.. (10^6-1)]
		let ans = foldr1 tupMax mCols
		writeFile "pe14.txt" $ show ans
		print ans