{-

It is easily proved that no equilateral triangle exists with integral length sides and integral area. However, the almost equilateral triangle 5-5-6 has an area of 12 square units.

We shall define an almost equilateral triangle to be a triangle for which two sides are equal and the third differs by no more than one unit.

Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and area and whose perimeters do not exceed one billion (1,000,000,000).

-}

{-

Consider the triangle (a,b,c) with an altitude of length h orthogonal to the side which is not equal to the other two sides

(1) Under the restriction of a=b and c=a+1, then we solve Pell's equation k^2-3h^2=1, k=(3a-1)
(1) Under the restriction of a=b and c=a-1, then we solve Pell's equation k^2-3h^2=1, k=(3a+1)

-}

-- Pull in functions from PE 66 to get the convergents of Pell's equation: x^2 - d*y^2 = 1

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

-- Gets the list of convergents of the square root of a number d
convergents :: Integer -> [(Integer,Integer)]
convergents d = zip (seqCF an 1 0) (seqCF an 0 1)  where
  an = lstFracs d

-- Get the set of Pell solutions (x,y) for the equation x^2-d*y^2=1
pells :: Integer -> [(Integer, Integer)]
pells d = (x1,y1) : nextPell (x1,y1)
    where
        (x1,y1) = head [(x,y) | (x,y) <- convergents d, x^2-d*y^2==1]
        next xk yk = (x1*xk+d*y1*yk, x1*yk+y1*xk)
        nextPell (xk,yk) = (next xk yk) : nextPell (next xk yk)

-- Return the side lengths of the almost equilateral triangles, split between the (a,a,a+1) and (a,a,a-1) types
aeTriangles :: [[Integer]]
aeTriangles =   [[a,a,a`f`1] |  (k,h) <- pells 3, f <- [(+),(-)],
                                ((2*k)`f`1)`mod`3 == 0, let a = ((2*k)`f`1)`div`3, a > 1]

-- Get the sum of the perimeters of all almost equilateral triangles whose perimeter is less than or equal to n
sumPerimeter :: Integer -> Integer
sumPerimeter n = sum . map sum $ takeWhile (\l -> sum l <= n) aeTriangles

-- Print and write
main :: IO()
main = do
        let ans  = sumPerimeter (10^9)
        writeFile "pe94.txt" $ show ans
        print ans
