-- Don't forget to compile with the -O flag (took 1503.56s to 1793.38s to compute anyways).

-- Import the naive square root function 
squareRoot :: Integer -> Integer
squareRoot = floor . sqrt . (fromIntegral :: Integer -> Double)

-- Create a helper function to deal with perfect squares
psq n xs
	| xs ^ 2 == n = xs
	| otherwise = xs + (n `div` xs)

-- Define an efficient proper divisors sum function
sumAliquot :: Integer -> Integer
sumAliquot n = - n + sum [psq n xs | xs <- [1.. (squareRoot n)], n `mod` xs == 0]

-- Create all abundant numbers
abNums = [n | n <- [1.. 28123], sumAliquot n > n]

-- Define a function to check if a number is abundant pair summable
pairAbSum n = pairAbSum' n abNums abNums n where
	pairAbSum' n lst1@(h1:t1) lst2@(h2:t2) limit
		| h1 + h2 == n = True
		| h1 > limit = False
		| h2 > limit || h2 + h1 > limit = pairAbSum' n t1 abNums limit
		| otherwise = pairAbSum' n lst1 t2 limit

-- Print and write out the answer 
main = do 
		let ans = sum [n | n <- [1..28123], pairAbSum n == False]
		writeFile "pe23.txt" $ show ans
		print ans