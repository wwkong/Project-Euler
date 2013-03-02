fib' = 0 : 1: zipWith (+) fib' (tail fib') 
main = print(sum(takeWhile (<4000000) (filter even fib')))
