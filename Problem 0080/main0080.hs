{-
It is well known that if the square root of a natural number is not an integer, then it is irrational. The decimal expansion of such square roots is infinite without any repeating pattern at all.

The square root of two is 1.41421356237309504880..., and the digital sum of the first one hundred decimal digits is 475.

For the first one hundred natural numbers, find the total of the digital sums of the first one hundred decimal digits for all the irrational square roots.
-}

import Data.List

-- Taken from the Haskell wiki
(^!) :: Num a => a -> Int -> a
(^!) x n = x^n
squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
   in  head $ dropWhile (not . isRoot) iters

-- For a given starting point (p,r) after the whole number estimate, get the infinite decimal expansion (based on the wiki algorithm)
dSqrtDec :: Integer -> Integer -> [Integer]
dSqrtDec p r = x : dSqrtDec (p*10+x) (c-y)
    where
        c  = r*100
        disc = 100*p^2+c
        x  = -10*p + squareRoot disc
        y  = x*(20*p + x)

-- Wrapper for the above to get the infinite [n,d] pair of whole number (n) and decimal expansion (d)
sqrtDec :: Integer -> [Integer]
sqrtDec n = p : dSqrtDec p r
    where
        p = squareRoot n
        r = n - (squareRoot n ^ 2)

-- Test out the given example
test :: Integer
test = sum $ take 100 $ sqrtDec 2

-- Generate the list of non-squares
nonSquares :: [Integer]
nonSquares = [x | x <- [1..100], squareRoot x ^ 2 /= x]

-- Print and write
main :: IO()
main = do
        let ans = sum $ map (sum . take 100 . sqrtDec) nonSquares
        writeFile "pe80.txt" $ show ans
        print ans
