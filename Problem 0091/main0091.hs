{-

There are exactly fourteen triangles containing a right angle that can be formed when each co-ordinate lies between 0 and 2 inclusive; that is,
0 ≤ x1, y1, x2, y2 ≤ 2.

Given that 0 ≤ x1, y1, x2, y2 ≤ 50, how many right triangles can be formed?

-}

-- Given a coordinate in the interior of a square, find all right triangles which have a base side as the line from the origin to the coordinate with boundaries (0,0) to (n,n) and return as the list of the third coordinate
baseTriangles :: (Int,Int) -> Int -> [(Int,Int)]
baseTriangles (x1,y1) n = [(x2,y2) | k <- [lowK..upK], let (x2,y2) = (x1+dx*k, y1-dy*k), 
                                     0 <= x2 && x2 <= n, 0 <= y2 && y2 <= n, k /= 0]
    where 
        gcdXY       = gcd x1 y1
        (dx, dy)    = (y1`div`gcdXY, x1`div`gcdXY) 
        (upK, lowK) = (max ((n-x1)`div`dx) ((n-y1)`div`dy), min (-x1`div`dx) (-y1`div`dy))

-- Wrapper to the above to find the set of baseTriangles for given n
lBaseTriangles :: Int -> [[(Int,Int)]]
lBaseTriangles n = [[(x1,y1),(x2,y2)] | x1 <- [1..n], y1 <- [1..n], (x2,y2) <- baseTriangles (x1,y1) n]

-- For a given dimension, get the total number of right triangles (3n^2 triangles have the property that at least one side is touching the edges)
triangles :: Int -> Int
triangles n = 3*n*n + length (lBaseTriangles n)

-- Print and write
main :: IO()
main = do
        let ans  = triangles 50
        writeFile "pe91.txt" $ show ans
        print ans