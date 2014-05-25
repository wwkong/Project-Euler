-- How many n-digit positive integers exist which are also an n-th power?

{-
Note that another way of phrasing this is how many integer pairs (a,n) have the property of
1 + (floor (n * logBase 10 a)) == n ->> floor (n * logBase 10 a) == n - 1. Here,
a^n is the number that has the property described in the question.

Note that this gives an upper bound for M = (logBase 10 a) as (n-2)/n < M < 1. This means that a < 10 and
since n and a are discrete, then (n-2)/n < logBase 10 9 ->> n < 2/(1 - logBase 10 9) ~= 43.07
-}

-- Define a function to check if a given (a,n) pair generate a "powerful number". That is,
-- if a^n has n digits
isPowerful :: Int -> Int -> Bool 
isPowerful a n = (length . show) (a ^ n) == n

-- Print and write out the answer
main :: IO()
main = do
        let ans = length [a^n | a <- [1..9], n <- [1..43], isPowerful a n]
        writeFile "pe63.txt" $ show ans
        print ans
