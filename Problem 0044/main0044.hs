{-
Find the pair of pentagonal numbers, Pj and Pk, for which their sum and difference are pentagonal and 
D = |Pk  Pj| is minimised; what is the value of D?
-}

{- 
Thanks goes out to a tip from xkcd. 

We first note that any two consecutive pentagonal numbers P(m-1), Pm will have a difference of 3m-2 using some 
basic arithmetic. In general, then, the difference between P(m-k) and Pm will be D(k,m) = k(3m-2) - (3(k-1)(k) `div` 2).
We also have the sum S(k,m) = (m(3*m-1) + (m-l)(3(m-k)-1)) `div` 2. 

Thus, the first instance where D(k,m) and S(k,m) are pentagonal numbers will be our upper bound.
This is because O(m) < D(k,m) < O(m^3).

We'll take a guess and say that the first instance is minimal without proving it
(This will be done in a later update)
-}

-- Import the naive integer square root function
squareRoot :: Integer -> Integer
squareRoot = floor . sqrt . (fromIntegral :: Integer -> Double)

-- First define a function to check if a number is pentagonal
-- Recall from a previous question that a number p is pentagonal when 1+24p is perfect square and
-- sqrt(1+24) + 1 `mod` 6 == 0
isPentagonal n 
	| (squareRoot det)^2 /= det = False
	| ((squareRoot det) + 1) `mod` 6 /= 0 = False
	| otherwise = True
	where det = 1+24*n
	
-- Define our S(k,m) and D(k,m) functions
dPent k m = k*(3*m-2) - (3*(k-1)*(k) `div` 2)
sPent k m = (m*(3*m-1) + (m-k)*(3*(m-k)-1)) `div` 2
	
-- Find our upper bound as a pair (m, D(k,m))
upBound = head [(m-k, m, dPent k m) | m <- [1..], k <- [1..(m-1)], isPentagonal (dPent k m) && isPentagonal (sPent k m)]

main = do 
		let ans = upBound
		writeFile "pe44.txt" $ show ans
		print ans