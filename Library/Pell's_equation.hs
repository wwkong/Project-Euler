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