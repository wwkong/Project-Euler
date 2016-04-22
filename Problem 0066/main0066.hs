{-
Consider quadratic Diophantine equations of the form:

x^2 – Dy^2 = 1

For example, when D=13, the minimal solution in x is 649^2 – 13×180^2 = 1.

It can be assumed that there are no solutions in positive integers when D is square.

By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:

3^2 – 2×2^2 = 1
2^2 – 3×1^2 = 1
9^2 – 5×4^2 = 1
5^2 – 6×2^2 = 1
8^2 – 7×3^2 = 1

Hence, by considering minimal solutions in x for D ≤ 7, the largest x is obtained when D=5.

Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is obtained.
-}

import           Data.List
import           Data.Ord
import           Data.Ratio

-- Return the list of {a_i} of the regular continued fraction representation of (sqrt x)
lstFracs :: Integer -> [Integer]
lstFracs x = a0 : iterFracs 0 1 a0 where
  a0 = (floor . sqrt .fromIntegral) x
  iterFracs :: Integer -> Integer -> Integer -> [Integer]
  iterFracs m d a = aNext : iterFracs mNext dNext aNext where
    mNext = d*a-m
    dNext = (x - mNext^2) `div` d
    aNext = floor (fromIntegral (a0+mNext)/fromIntegral dNext)

-- Builds the fundamental recurrence relationship components
seqCF :: [Integer] -> Integer -> Integer -> [Integer]
seqCF an m1 m2 = seqCFn : seqCF (tail an) seqCFn m1 where
  seqCFn = head an * m1 + m2

-- Gets the list of convergents of the square root of a number x
convergents :: Integer -> [Ratio Integer]
convergents x = zipWith (%) (seqCF an 1 0) (seqCF an 0 1)  where
  an = lstFracs x

-- Get non-square numbers from PE 80
nonSquares :: [Integer]
nonSquares = [x | x <- [1..1000], ((^2) . floor . sqrt . fromIntegral) x /= x]

-- Use the convergents to solve Pell's equation for a given D and return the first numerator
-- This is because we know there is some convergent a/b for sqrt(D) such that a^2-D*b^2 = 1
minX :: Integer -> Integer
minX d = head [a |  r <-  convergents d,
                          let a = numerator r,
                          let b = denominator r,
                          a^2 - d*b^2 == 1]

-- Print and write
main :: IO()
main = do
        let ans = fst $ maximumBy (comparing snd) [(d, minX d) | d <- nonSquares]
        writeFile "pe66.txt" $ show ans
        print ans
