pythagTriBase :: Int -> [[Int]]
pythagTriBase s = [[a,b,c] |   m <- [1..s] :: [Int]
                             , n <- [1..minimum [(m-1), s `div` (2*m)]] :: [Int]
                             , k <- [1..minimum [s `div` (2*m*n), s `div` (m*m - n*n)]]
                             , gcd m n == 1
                             , (m - n) `mod` 2 == 1
                             , let a = k * (m*m - n*n)
                             , let b = k * (2*m*n)
                             , let c = k * (m*m + n*n)
                             , a < s, b < s, a > 0, b > 0 ]