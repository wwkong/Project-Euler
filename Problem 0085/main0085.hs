{-
By counting carefully it can be seen that a rectangular grid measuring 
3 by 2 contains eighteen rectangles.

Although there exists no rectangular grid that contains exactly two million 
rectangles, find the area of the grid with the nearest solution.
-}

-- Explanation

{-
It is obvious that given a largest side length k, that a square would be the shape
that has the largest number of contained subrectangles. Thus, if U(n) is the number
of subrectangles in the n by n square, then it is an easy inductive argument to show that
U(n) is an upper bound for the number of subrectangles in an a by b sized rectangle,
a <= n, b <= n.

It is also to see graphically that U(n) = sum [k * j | k <- [1..n], j <- [1..n]] simply
by considering all possible bottom right corners that a subrectangle can take. This is further 
simplified to (sum [k | k <- [1..n]) ^ 2 = (n(n+1)/2) ^ 2 using double summation rules

Similarly, a single row of length k would be the shape to generate a lower bound of 
number of subrectangles for given largest side k. Using similar logic as above, the 
lower bound is L(n) = n(n+1)/2

This means that our starting dimension is n_L ~= 53 and upper bound is 
n_U ~= 2000 where U(n_L) ~= 2*10^6 ~= L(n_U)
-}

import Data.List

-- Define a preset tolerance level (subject to change)
tol = 1000

-- First define a function to compute the number of subrectangles in 
-- an a by b sized rectangle
subRects a b = (a*(a+1) `div` 2) * (b*(b+1) `div` 2)

-- Find all subrectangle counts within the tolerance level above from 2*10^6 for
-- a given maximum side length along with the area of the corresponding rectangle
-- (Note that we only recurse on one side due to symmetry)
subCounts n = [(k*n,count) | k <- [1..n], let count = subRects k n, count > 2*10^6 - tol, count < 2*10^6 + tol]

-- Here I just realized that the maximumBy and minimumBy function do exactly
-- what my tupMax and tupMin functions do OTZ

-- Print and write out the answer
main = do
		let allCounts = concat [subCounts n | n <- [53..2000]]
		let ans = fst $ minimumBy 	(\(a,b) (c,d) -> (compare b d)) 
									[(area, abs (2*10^6-c)) | t <- allCounts, let c = snd t, let area = fst t]
		writeFile "pe85.txt" $ show ans
		print ans