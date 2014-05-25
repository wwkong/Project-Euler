{-
In this problem, proper fractions are listed by n/d where gcd(n,d) == 1.

By listing the set of reduced proper fractions for d <= 1,000,000 in ascending order
of size, find the numerator of the fraction immediately to the left of 3/7.
-}

import           Data.Ratio

-- Haskell has a really nice library for this kind of problem

-- Create a list of possible candidates, with our bound being
-- [(d * 3) `div` 7 - 1, (d * 3) `div` 7] for n
beside :: [Ratio Integer]
beside = [n % d |  d <- [1..10^6], n <- [(d*3) `div` 7 - 1, (d*3) `div` 7] , n % d < 3 % 7]

-- We fold over our possibilities using the max function to get our answer

-- Print and write out the answer
main :: IO()
main = do
        let ans = numerator (foldl1 max beside)
        writeFile "pe71.txt" $ show ans
        print ans
