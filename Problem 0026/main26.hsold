import Debug.Trace

-- Create a function that converts a fraction of the 
-- form 1/b to its decimal expansion in list form
toDecLst b = take b $ tail $ toDecLst' 1 b where
	toDecLst' a b
		| a == b = [1] ++ (repeat 0)
		| a < b = 0 : toDecLst' (a*10) b 
		| a `mod` b == 0 = [a `div` b] ++ (repeat 0)
		| a > b = (a `div` b) : toDecLst' (a `mod` b * 10) b 
		
-- Implement a naive version of Brent's algorithm to find a cycle length
mBrentCycle :: [Int] -> Integer
mBrentCycle dec = mBrentCycle' 0 0 1 0 where
	mBrentCycle' t h lim lam 
		-- | trace ("Called with t=" ++ show t ++ " h=" ++ show h ++ " lim=" ++ show lim ++ " lam=" ++ show lam) False = undefined
		| t == h = mBrentCycle' t (h+1) lim (lam+1) 
		| dec !! t == dec !! h = lam
		| lim == lam = mBrentCycle' h h (lim*2) 0 -- Teleport the turtle
		| otherwise = mBrentCycle' t (h+1) lim (lam+1)
		
-- test n = mBrentCycle (toDecLst n)