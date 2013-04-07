{-
The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting
to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by
cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than one in 
value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms, find the 
value of the denominator.
-}

import Data.Char
import Data.List

-- Define a function to do the naive method of division 
-- Input will be in the form of an ordered pair
naiveCurious (a,b)
	| '0' `elem` aStr || '0' `elem` bStr = (1,1) -- Special pair for trivial cases
	| aStr !! 0 == bStr !! 0 = (digitToInt (aStr !! 1), digitToInt (bStr !! 1))
	| aStr !! 0 == bStr !! 1 = (digitToInt (aStr !! 1), digitToInt (bStr !! 0))
	| aStr !! 1 == bStr !! 1 = (digitToInt (aStr !! 0), digitToInt (bStr !! 0))
	| aStr !! 1 == bStr !! 0 = (digitToInt (aStr !! 0), digitToInt (bStr !! 1))
	| otherwise = (1,1) -- Another special case for non-reducible pairs
	where 
		aStr = show a
		bStr = show b
		
-- Define a function to check whether or not one ordered pair is a reduced fraction 
-- of the other
isReduced (a,b) (c,d) = a*d == b*c

-- Find the 4 fractions which are curious
cFracs = [ (a,b) | a <- [10.. 99], b <- [10..99], a < b, isReduced (a,b) (naiveCurious (a,b)) ]

-- Find the product
ansFrac = foldr1 (\a b -> ((fst a) * (fst b), (snd a) * (snd b))) cFracs  

-- Print and write out the answer
main = do
		let ans = (snd ansFrac) `div` (gcd (fst ansFrac) (snd ansFrac))
		writeFile "pe33.txt" $ show ans
		print ans