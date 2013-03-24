-- Define a memoized fib function
fib' = 0 : 1: zipWith (+) fib' (tail fib') 
<<<<<<< HEAD
main = print (sum (takeWhile (< 4000000) (filter even fib')))
=======

-- Print and write out the answer
main = do
		let ans = sum $ takeWhile (<4000000) (filter even fib')
		writeFile "pe2.txt" $ show ans
		print ans
>>>>>>> Modified existing Haskell solutions
