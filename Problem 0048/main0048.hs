-- Set up a list [1.. 1000] that filters out multiples of 10 > 10 since these do not affect sum
mList = [xs | xs <- [1.. 1000], xs `mod` 10 /= 0]

-- Set up a somewhat efficient multiplication function
mTimes n = mTimes' n n n where
    mTimes' n m p
		| m == 1 = n
		| otherwise = mTimes' (n*p `mod` 10^11) (m-1) p
		
-- Print and write out the answer
main = do 
		let ans = foldr1 (\a b -> (a + b) `mod` 10^11) $ map mTimes mList
		writeFile  "pe48.txt" $ show ans 
		print ans