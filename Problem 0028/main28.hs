{-
It can be shown that given a side length n of the sqaure, the corners of the square will
have sum: 4*cn + (1+2+3)*(n-1) = 4*cn + 6*(n-1) where cn is the maximum valued corner of the
(n-1) sized sub-square plus (n-1).

To add, the smallest valued corner of the n sized square is the value of the 
largest corner of the (n-1) sized sub-square which can be caclulated as 
3*((n-2)-1) + ((lastSum - 6*((n-2)-1)) `div` 4) + (n-1).

The elements of the n sized square form an arithmetic sequence with difference 2*(n-1).
-}

-- Define a function to calculate the sum of the cross diagonal
crossDiagSum :: Integer -> Integer
crossDiagSum n
	| even n = error "You entered an even side length!"
	| n == 1 = 1
	| n == 3 = 24
	| otherwise =  4*cn + 6*(n-1)
	where
		lastSum = crossDiagSum (n-2)
		cn = 3*((n-2)-1) + ((lastSum - 6*((n-2)-1)) `div` 4) + (n-1)

-- Print and write out the answer
main = do
		let ans = sum $ map crossDiagSum $ filter odd [1..1001]
		writeFile "pe28.txt" $ show ans
		print ans