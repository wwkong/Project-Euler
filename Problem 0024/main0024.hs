

-- What is the millionth lexicographic permutation of the digits
-- 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

import          Data.Word

-- Full credit goes to 
-- http://nayuki.eigenstate.org/page/next-lexicographical-permutation-algorithm
-- for the algorithm used

-- Find the length of the longest non-decreasing sequence in a list
nDecSeqLen :: [Word] -> Int
nDecSeqLen (xh:xt)
    | xt == [] = 1
    | xh <= head xt = 1 + nDecSeqLen xt
    | otherwise = 1


-- Get the next permuation
nextPermutation :: [Word] -> [Word]
nextPermutation p =  (init prfx ++ [last mid] ++ rev)
    where   len = length p
            sufxLen = nDecSeqLen (reverse p)
            prfx = take (len-sufxLen) p
            sufx = drop (len-sufxLen) p 
            mid = takeWhile (>= last prfx) sufx 
            rev = reverse $ init mid ++ [last prfx] ++ drop (length mid) sufx

-- Print and write out the answer
main :: IO()
main = do
        let ansLst = iterate nextPermutation [0..9] !! (10^6-1)
        let ans = (concat . map show) ansLst
        writeFile "pe24.txt" ans
        print (read ans :: Integer)