-- Find the difference between the sum of the squares of the first one 
-- hundred natural numbers and the square of the sum.


-- Print and write out the answer
main = do 
		let ans = sum [x*y | x <- [1..100], y <- [1..100], x /= y] 
		writeFile "pe6.txt" $ show ans
		print ans