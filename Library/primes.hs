-- Prime Sieve from Jeffrey Negrea

import Data.List 

squareRoot :: Int -> Int
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r = r^2 <= n && n < (r+1)^2
  in head $ dropWhile (not . isRoot) iters

primesHelper [] m sqrtM =[]
primesHelper [a] m sqrtM =[a]
primesHelper (a:xs) m sqrtM
  |a>(sqrtM+1) =a:xs
  |otherwise =a:(primesHelper (filter (\x-> mod x a /=0) xs) m sqrtM)

primes m = (primesHelper (2:[3,5..m]) m (squareRoot m))

-- Given an integer n, find the set of all factors of n
factorsN :: Int -> [Int]
factorsN n =  sort $ foldl (\l1 l2 -> [a*b|a<-l1,b<-l2]) [1] pMult
    where 
        pFact = group (primeFactors n)
        pDecmp = zip (map head pFact) (map length pFact)
        pMult = map (\(p,e) -> [p^k | k <- [0..e]]) pDecmp