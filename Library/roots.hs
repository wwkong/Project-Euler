-- Function to find the integral kth root of a number n
intKRoot :: Int -> Int -> Int
intKRoot k n = -1 + head [a | a <- [lbound..], a ^ k > n]
    where
        logBase2 x = finiteBitSize x - 1 - countLeadingZeros x
        lbound = 2 ^ ((logBase2 n) `div` k) :: Int