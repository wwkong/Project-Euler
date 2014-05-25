{-
Define a reduced fraction as n/d for n<d and GCD(n,d) = 1

How many elements would be contained in the set of reduced
proper fractions for d<=1,000,000?
-}

import           Math.Sieve.Phi

-- Print and write out the answer
main :: IO()
main = do
        let lim = 10^6
        let mySieve = sieve lim
        let ans = (sum $ map (phi mySieve) [1..lim]) - 1
        writeFile "pe72.txt" $ show ans
        print ans
