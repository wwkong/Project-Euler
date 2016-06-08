-- Integer Square root
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r = r^2 <= n && n < (r+1)^2
  in head $ dropWhile (not . isRoot) iters


-- Function to find the integral kth root of a number n
intKRoot :: Int -> Int -> Int
intKRoot k n = -1 + head [a | a <- [lbound..], a ^ k > n]
    where
        logBase2 x = finiteBitSize x - 1 - countLeadingZeros x
        lbound = 2 ^ ((logBase2 n) `div` k) :: Int