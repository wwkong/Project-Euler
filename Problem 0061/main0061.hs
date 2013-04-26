{-
The ordered set of three 4-digit numbers: 8128, 2882, 8281, has three interesting properties.

The set is cyclic, in that the last two digits of each number is the first two digits of the 
next number (including the last number with the first).

Each polygonal type: triangle (P[3,127]=8128), square (P[4,91]=8281), and pentagonal 
(P[5,44]=2882), is represented by a different number in the set.

This is the only set of 4-digit numbers with this property.

Find the sum of the only ordered set of six cyclic 4-digit numbers for which each polygonal 
type: triangle, square, pentagonal, hexagonal, heptagonal, and octagonal, is represented by 
a different number in the set.
-}

import Data.List

-- Import the naive square root function
squareRoot :: Integer -> Integer
squareRoot = floor . sqrt . (fromIntegral :: Integer -> Double)

-- Using the concepts in Problem #45, we import a couple is[Figurate] functions
isTriangular n = let d = 1+8*n in isSquare d && (squareRoot d + 1) `mod` 2 == 0
isSquare n = (squareRoot n) ^ 2 == n
isPentagonal n = let d = 1+24*n in isSquare d && (squareRoot d + 1) `mod` 6 == 0
isHexagonal n = let d = 1+8*n in isSquare d && (squareRoot d + 1) `mod` 4 == 0

-- Using the same logic we get the conditions for the Heptagonal and Octagonal numbers below
isHeptagonal n = let d = 9+40*n in isSquare d && (squareRoot d + 3) `mod` 10 == 0
isOctagonal n = let d = 4+12*n in isSquare d && (squareRoot d + 2) `mod` 6 == 0

-- Build up our inventory of functions
fnInvInit = [isTriangular, isSquare, isPentagonal, isHexagonal, isHeptagonal, isOctagonal]

-- Given a function inventory and a number, return a new inventory with the first function 
-- application that returns True removed
nextInv n inv = let fn = head [f | f <- inv, f n] in inv
