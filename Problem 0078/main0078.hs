{-
Let p(n) represent the number of different ways in which n coins can be separated into piles. 
For example, five coins can be separated into piles in exactly seven different ways, so p(5)=7.

OOOOO
OOOO   O
OOO   OO
OOO   O   O
OO   OO   O
OO   O   O   O
O   O   O   O   O

Find the least value of n for which p(n) is divisible by one million.
-}

import Data.Array

-- We use the generating function for partitions which is of the form 1/P(x)
-- From Wikipedia, P(x) comes from Euler's partition formula

-- Return the power of the i-th term in P(x), zero indexed
powEuler :: Integer -> Integer
powEuler k
    | k == 0    = 0
    | odd k     = (3*n^2-n) `div` 2
    | even k    = (3*n^2+n) `div` 2
    | otherwise = 0   
    where
        n = (k+1) `div` 2

-- Store the pentagonal info
pentagonals :: [Integer]
pentagonals      = map powEuler [1..]
pentagonalSgns :: [Integer]
pentagonalSgns  = cycle [1,1,-1,-1]

-- Count the number of partitions of n using p(n) = p(n-1) + p(n-2) + ... + sgnEuler(k)*p(n-powEuler(k))
partition n
    | n <= 1    = 1
    | otherwise = sum [sgn * (cacheParts ! (n - k)) | (sgn, k) <- zip pentagonalSgns (takeWhile (<= n) pentagonals)]

-- Cache an array of an arbitrary size
cacheParts :: Array Integer Integer
cacheParts = array (0, 200000) [(n, partition n) | n <- [0..200000]]

-- Print and write out the answer
main :: IO()
main = do
        let ans = head [i | (i, p) <- assocs cacheParts, p `mod` 10^6 == 0]
        writeFile "pe78.txt" $ show ans
        print ans
