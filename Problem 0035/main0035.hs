-- Compile with the -O flag for efficiency

-- import Debug.Trace
import Data.Numbers.Primes
import Data.Array
import qualified Data.MemoCombinators as Memo

-- Define a function to check if a prime is circular
isCircular n = Memo.integral isCircular' n n True where
	isCircular' n cur bool 
		-- | trace ("Called with " ++ show n ++ " " ++ show cur ++ " " ++ show bool) False = undefined
		| bool == False = False
		| length curStr < (length $ show n) = False
		| next == n = bool && isPrime cur
		| otherwise = isCircular' n next (bool && isPrime cur)
		where 
			next = read ((tail curStr) ++ [head curStr]) :: Integer
			curStr = show cur
			
-- Print and write out the answer
main = do
		let ans = length [n | n <- takeWhile (<10^6) primes, isCircular n]
		writeFile "pe35.txt" $ show ans
		print ans