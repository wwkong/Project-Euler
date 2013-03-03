-- All credit goes to CloudiDust
sieve (x:xs) = x : (sieve (filter (\y -> y `mod` x /= 0) xs))
primes' = sieve [2..]
findMaxPrimeFactor n = findMaxPrimeFactor' n primes'
findMaxPrimeFactor' 1 (x:xs) = x
findMaxPrimeFactor' n (x:xs) | n `mod` x == 0 = findMaxPrimeFactor' (n `div` x) (x:xs)
                             | otherwise      = findMaxPrimeFactor' n xs
main = print (findMaxPrimeFactor 600851475143)