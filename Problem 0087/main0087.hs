{-
The smallest number expressible as the sum of a prime square, prime cube, and prime fourth power is 28. In fact, there are exactly four numbers below fifty that can be expressed in such a way:

28 = 2^2 + 2^3 + 2^4
33 = 3^2 + 2^3 + 2^4
49 = 5^2 + 2^3 + 2^4
47 = 2^2 + 3^3 + 2^4

How many numbers below fifty million can be expressed as the sum of a prime square, prime cube, and prime fourth power?
-}

-- Credits to the Haskell wiki for the hints

import           Data.Array.Unboxed
import           Data.List
import           Data.Numbers.Primes

-- Take the floored n-th root of a number
nroot :: Int -> Int -> Int
nroot n x = round (fromIntegral x ** (1/(fromIntegral n)))

-- Floored prime list
fprimes n k = takeWhile (<= nroot n k) primes

-- Get an association list of the numbers which match the specification
getPrimeLst :: Int -> UArray Int Bool
getPrimeLst n = accumArray (||) False (1,n) [(a+d,True) | a <- squares, d <- pairs, a+d<n]
    where
        squares = map (^2) (fprimes 2 n)
        cubes   = map (^3) (fprimes 3 n)
        fourths = map (^4) (fprimes 4 n)
        pairs   = nub [b+c | c <- fourths, b <- takeWhile (<n-c) cubes]

-- Try the given example
test :: Int
test = length $ filter id $ elems (getPrimeLst 50)

-- Print and write
main :: IO()
main = do
        let ans = length $ filter id $ elems (getPrimeLst (50*10^6))
        writeFile "pe87.txt" $ show ans
        print ans
