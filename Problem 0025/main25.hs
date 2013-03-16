-- Failed. Full credit goes to "Implements Developer {"

-- Define a memoized fib
fibonacciNumbers = 0:1:zipWith (+) fibonacciNumbers (tail fibonacciNumbers)

limit = 10^999
index = length w where w = takeWhile (< limit) fibonacciNumbers

main = print index