{-

A natural number, N, that can be written as the sum and product of a given set of at least two natural numbers, {a1, a2, ... , ak} is called a product-sum number: N = a1 + a2 + ... + ak = a1 × a2 × ... × ak.

For example, 6 = 1 + 2 + 3 = 1 × 2 × 3.

For a given set of size, k, we shall call the smallest N with this property a minimal product-sum number. The minimal product-sum numbers for sets of size, k = 2, 3, 4, 5, and 6 are as follows.

k=2: 4 = 2 × 2 = 2 + 2
k=3: 6 = 1 × 2 × 3 = 1 + 2 + 3
k=4: 8 = 1 × 1 × 2 × 4 = 1 + 1 + 2 + 4
k=5: 8 = 1 × 1 × 2 × 2 × 2 = 1 + 1 + 2 + 2 + 2
k=6: 12 = 1 × 1 × 1 × 1 × 2 × 6 = 1 + 1 + 1 + 1 + 2 + 6

Hence for 2≤k≤6, the sum of all the minimal product-sum numbers is 4+6+8+12 = 30; note that 8 is only counted once in the sum.

In fact, as the complete set of minimal product-sum numbers for 2≤k≤12 is {4, 6, 8, 12, 15, 16}, the sum is 61.

What is the sum of all the minimal product-sum numbers for 2≤k≤12000?

-}

import Data.Bits
import Data.Array
import Data.List

-- (1) Search space of candidate product-sum numbers of order k: [k,2k] (can be derived algebraically)
-- (2)  A set of factors can be mapped to a product sum via:
--      * N = prod(factors) = 1+1+..+1+sum(factors) with N - sum(factors) multiples of 1 
--      * This set is of size N - sum(factors) + length(factors)

-- Return the set of factorizations of set size k and numbers with products less than n
factorize :: Int -> Int -> [[Int]]
factorize n 1 = [[i] | i <- [2..n]]
factorize n k = [i:xs | i <- [2..n], xs <- factorize (n `div` i) (k-1)]

-- Return the set of factorizations with products less than x
factorsLE :: Int -> [[Int]]
factorsLE x = concat ([factorize x k | k <- [2..kBound]])
    where 
        kBound = head [k | k <- [1..], 2 ^ k > x]

-- Given a factorization, return the order of the factor set which generates a product sum number
orderF :: [Int] -> Int
orderF f = product f - sum f + length f 

-- Find the set of minimal product-sums for all 2≤k≤n
minProdSum :: Int -> Int
minProdSum n = sum arrOrd
    where 
        arrOrd = accumArray (min) (2*n) (2,n) [(k, product f) | f <- facts, let k = orderF f, k <= n]
        facts = factorsLE (2*n)

-- Print and write
main :: IO()
main = do
        let ans  = minProdSum 24000
        writeFile "pe88.txt" $ show ans
        print ans