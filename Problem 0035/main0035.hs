{-
The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

How many circular primes are there below one million?
-}

-- Compile with the -O flag for efficiency

-- import Debug.Trace
import           Data.Array
import qualified Data.MemoCombinators as Memo
import           Data.Numbers.Primes

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
