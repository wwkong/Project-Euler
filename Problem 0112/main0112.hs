{-
Working from left-to-right if no digit is exceeded by the digit to its left 
it is called an increasing number; for example, 134468.

Similarly if no digit is exceeded by the digit to its right it is called
a decreasing number; for example, 66420.

We shall call a positive integer that is neither increasing nor decreasing a 
"bouncy" number; for example, 155349.

Clearly there cannot be any bouncy numbers below one-hundred, but just over 
half of the numbers below one-thousand (525) are bouncy. In fact, the least 
number for which the proportion of bouncy numbers first reaches 50% is 538.

Surprisingly, bouncy numbers become more and more common and by the time we 
reach 21780 the proportion of bouncy numbers is equal to 90%.

Find the least number for which the proportion of bouncy numbers is exactly 99%.
-}

import Data.List
import Data.Ratio
import Debug.Trace

-- First time using custom data types!
data NumType = Increasing | Decreasing | Bouncy deriving (Eq)

-- Create a function to get a number's NumType
getNumType :: Integer -> NumType
getNumType n 
	| smallN == show n = Increasing
	| bigN == show n = Decreasing
	| otherwise = Bouncy
	where 
		smallN = sort (show n)
		bigN = reverse smallN

-- For debugging purposes only
showNumType nType = case nType of
	Increasing -> "Increasing"
	Decreasing -> "Decreasing"
	Bouncy -> "Bouncy"
	
-- Iterate until we pass a threshold
iterBouncy limit = iterBouncy' 0 0 1 where 
	iterBouncy' a b n
		-- | trace ("a / b = " ++ show a ++ " " ++ show b) False = undefined
		| b == 0 = iterBouncy' a (b + 1) (n + 1) -- Special case
		| a % b >= limit = (n - 1)
		| nType == Bouncy = iterBouncy' (a + 1) (b + 1) (n + 1)
		| otherwise = iterBouncy' a (b + 1) (n + 1)
		where
			nType = getNumType n
		
-- Print and write out the answer
main = do
		let ans = iterBouncy (99 % 100)
		writeFile "pe112.txt" $ show ans
		print ans