-- What is the first term in the Fibonacci sequence to contain 1000 digits?

-- Full credit goes to "Implements Developer {" and the Haskell wiki

-- Define a memoized fib and set the limit
fib' = 0:1:zipWith (+) fib' (tail fib')
limit = 10^999

-- Print and write out the answer
main = do
        let ans = length w where w = takeWhile (< limit) fib'
        writeFile "pe25.txt" $ show ans
        print ans
