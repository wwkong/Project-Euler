-- What is the first term in the Fibonacci sequence to contain 1000 digits?

-- Full credit goes to "Implements Developer {" and the Haskell wiki

-- Define a memoized fib and set the limit
fib' :: [Integer]
fib' = 0:1:zipWith (+) fib' (tail fib')

-- Print and write out the answer
main :: IO()
main = do
        let ans = length w where w = takeWhile (< 10^999) fib'
        writeFile "pe25.txt" $ show ans
        print ans
