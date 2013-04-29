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
import Data.Maybe
import Control.Applicative
import Debug.Trace

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

-- Wrap our fig type functions in a list
fnLst = [isTriangular, isSquare, isPentagonal, isHexagonal, isHeptagonal, isOctagonal]

-- Build up our figurate numbers
figNums n = [k | k <- [1000..9999], (fnLst !! n) k]
figNumLst = [lst | lst <- [figNums n | n <- [0..4]]] -- Octagonal numbers will be left out

-- Build a lst drop function by index
dropLst lst n = take n lst ++ drop (n+1) lst

-- Generate a unique cycle of length k with starting number n, and candidate list of lists, lst, 
-- and an initial index, indx
genCycle k n lst = genCycle' k n lst 0 where
	genCycle' k' n' lst' indx
		-- | trace ("genCycle " ++ show k' ++  " " ++ show n' ++ " " ++  show indx ++ " " ++ show filLst) False = undefined
		| k' == 1 = [n'] 
		| indx == length lst' = [] 
		-- We reached the end of the candidate list, return empty
		| null curSubLst = 
			genCycle' k' n' lst' (indx+1) 
		-- Our first try didn't work, keep going in the main list
		-- Cycle of length 1
		| k' == 2 && tailCond curElem n =
			[n'] ++ (genCycle' (k'-1) curElem  nextMainLst 0) 
		-- Special terminating case
		| k' == 2 && not (tailCond curElem  n) = 
			genCycle' k' n' nextSubLst indx 
		-- Special case to achieve termination		
		| k' > (1 + length (genCycle' (k'-1) curElem nextMainLst 0)) =
			genCycle' k' n' nextSubLst indx
		-- Chain length check
		| otherwise =
			[n'] ++ genCycle' (k'-1) curElem nextMainLst 0
		-- Found a candidate; destroy sublist and advance
		where 
			tailCond a b = drop 2 (show a) == take 2 (show b)
			filLst = map (\nums -> (filter (\m -> tailCond n' m) nums)) lst'
			curSubLst = filLst !! indx
			curElem = head curSubLst
			nextSubLst = take indx lst' ++ [(lst' !! indx) \\ [curElem]] ++ drop (indx+1) lst'
			nextMainLst = dropLst lst' indx
		
main = do
		let ans = sum $ head [cycles | cycles <- [genCycle 6 n figNumLst | n <- figNums 5], not (null cycles)]
		writeFile "pe61.txt" $ show ans
		print ans