{-
It can be seen that n=6 produces a maximum n/φ(n) for n <= 10.
Find the value of n <= 1,000,000 for which n/φ(n) is a maximum.
-}

import qualified Math.Sieve.Phi as P

-- Here, we can just use a library hack to produce the answer

-- Import our tuple max function
tupMax :: (Double,Int) -> (Double,Int) -> (Double,Int)
tupMax a b
    | max (fst a) (fst b) == fst a = a
    | max (fst a) (fst b) == fst b = b

-- Print and write out the answer
main :: IO()
main = do
        let mySieve = P.sieve (10^6)
        let ans = foldr1 tupMax [(fromIntegral n / fromIntegral phiN, fromIntegral n) |
                                 n <- [1..10^6] :: [Integer], let phiN = P.phi mySieve n]
        writeFile "pe69.txt" $ show ans
        print ans
