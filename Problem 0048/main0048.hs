-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

-- Set up a list [1.. 1000] that filters out multiples of 10 > 10 since these do not affect sum
mList :: [Int]
mList = [xs | xs <- [1.. 1000], xs `mod` 10 /= 0]

-- Set up a somewhat efficient multiplication function
mTimes :: Int -> Int
mTimes n = mTimes' n n n where
    mTimes' p q r
        | q == 1 = p
        | otherwise = mTimes' (p*r `mod` 10^11) (q-1) r

-- Print and write out the answer
main :: IO()
main = do
        let ans = foldr1 (\a b -> (a + b) `mod` 10^11) $ map mTimes mList
        writeFile  "pe48.txt" $ show ans
        print ans
