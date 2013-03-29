-- Full credit goes to "Implements Developer {" and the Haskell wiki
-- Took 0.02s to compute.

-- Define a memoized fib and set the limit
fibonacciNumbers = 0:1:zipWith (+) fibonacciNumbers (tail fibonacciNumbers)
limit = 10^999

-- Print and write out the answer
main = do 
		let ans = length w where w = takeWhile (< limit) fibonacciNumbers
		writeFile "pe25.txt" $ show ans
		print ans		