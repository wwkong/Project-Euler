--In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
-- How many different ways can £2 be made using any number of coins?

-- Inspired by this article: https://subjoin.net/misc/m496pres1.nb.pdf
-- Don't forget to compile with the -O flag!

import           Data.Maybe
import           Data.MemoCombinators as Memo
import           Debug.Trace

-- First, create a dictionary to define the coin type and value
cTypeVal = [(1,1),(2,2),(3,5),(4,10),(5,20),(6,50),(7,100),(8,200)]

-- Create a function to count the number of ways recursively (memoized)
-- numways' n k is the number of ways to get n with the first k types of coins
numWays n = Memo.integral numWays' n 8 where
    numWays' n k
        -- | trace ("Called with n=" ++ show n ++ ", k=" ++ show k) False = undefined
        | n == 0 = 1
        | (k < 1) || (n < 0) = 0
        | otherwise = (numWays' n (k-1)) + (numWays' (n - ak) k)
            -- The first expr. is the number of ways where we don't use the k-th coin and the second
            -- is the number of ways in which do use it at least once
        where ak = fromJust $ lookup k cTypeVal

-- Print and write out the answer
main = do
        let ans = numWays 200
        writeFile "pe31.txt" $ show ans
        print ans
