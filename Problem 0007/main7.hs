-- Yay! First time implementing my own prime generator!
-- FAILED. Credit goes to CloudiDust for this one
sieve (x:xs) = x : (sieve (filter (\y -> y `mod` x /= 0) xs))
main = print $ sieve [2..] !! (10001 - 1) -- To account for the indexing