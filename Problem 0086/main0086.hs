{-

A spider, S, sits in one corner of a cuboid room, measuring 6 by 5 by 3, and a fly, F, sits in the opposite corner. By travelling on the surfaces of the room the shortest "straight line" distance from S to F is 10.

However, there are up to three "shortest" path candidates for any given cuboid and the shortest route doesn't always have integer length.

It can be shown that there are exactly 2060 distinct cuboids, ignoring rotations, with integer dimensions, up to a maximum size of M by M by M, for which the shortest route has integer length when M = 100. This is the least value of M for which the number of solutions first exceeds two thousand; the number of solutions when M = 99 is 1975.

Find the least value of M such that the number of solutions first exceeds one million.

-}

-- Generate the set of pythagorean triples with base side lengths at most s
pythagTriBase :: Int -> [(Int, Int, Int)]
pythagTriBase s = [ (p,q,r) |  m <- [1..s] :: [Int]
                             , n <- [1..minimum [m-1, s `div` (2*m)]] :: [Int]
                             , k <- [1..minimum [s `div` (2*m*n), s `div` (m*m - n*n)]]
                             , gcd m n == 1
                             , (m - n) `mod` 2 == 1
                             , let a = k * (m*m - n*n)
                             , let b = k * (2*m*n)
                             , let c = k * (m*m + n*n)
                             , a < s, b < s, a > 0, b > 0
                             , let p = min a b
                             , let q = max a b
                             , let r = c]

-- For a given pythagorean triple and upper bound M, get the number cuboids associated with it, where the optimal path is the hypotenuse and the sides are creases in the cuboids base map
cuboids :: Int -> (Int, Int, Int) -> Int
cuboids m (p,q,_)
    | q <= m = (p`div`2) + max (q`div`2+1-(q-p)) 0 -- (q `div` 2  + 1) is the number of big sub-lengths and (q-p) ensures that the big side length is not larger than the small side length 
    | p <= m = max ((min m (q`div`2))-(q-p)+1) 0 -- force p to be the smaller side
    | otherwise = 0


-- Find the number of cuboids with integral optimal path for a given side length bound M
nCuboids :: Int -> Int
nCuboids m = sum $ map (cuboids m) (pythagTriBase (m*2)) 

-- Print and write
main :: IO()
main = do
        let ans  = head [m | m <- [100..], let n = nCuboids m, n > 1000000]
        writeFile "pe86.txt" $ show ans
        print ans