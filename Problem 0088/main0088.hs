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

-- (1) Primes are excluded from the search
-- (2) 

import           Data.Array
import           Data.Maybe (fromJust, isNothing, isJust)
import           Data.Numbers.Primes (primeFactors)

-- For a given number n, generate the association list of entries into the final array
assocN :: Int -> [(Int, Maybe Int)]
assocN 1 = [(1, Just 0)]
assocN n = (k0, Just n) : [(k, Just 0) | k <- ks]
    where
        -- facts = primeFactors n
        -- nFacts = [2..(length pFacts)+1]
        (k0:ks) = zipWith (\nF sF -> (nF + n - (n `div` sF) - sF)) nFacts (scanl1 (+) facts)

-- Get the array of minimal product-sum numbers for 1≤k≤n without duplication
arrMinProdSum :: Int -> Array Int (Maybe Int)
arrMinProdSum n = accumArray accFun Nothing (1,n) [(k,m) | (k,m) <- (concat $ map assocN [2..(2*n)]), k <= n]
    where
        accFun m1 m2
            | isJust m1    = m1
            | otherwise    = m2

-- Wrapper to the above to sum the array for 2≤k≤n
sumMinProdSum :: Int -> Maybe Int
sumMinProdSum n = pure (-) <*> mSum <*> (mpsArr ! 1)
    where 
        mpsArr =(arrMinProdSum n)
        mSum = foldl (\x y -> pure (+) <*> x <*> y) (Just 0) mpsArr

-- Print and write
main :: IO()
main = do
        let ans  = 1
        writeFile "pe88.txt" $ show ans
        print ans