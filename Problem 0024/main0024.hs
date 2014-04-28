-- What is the millionth lexicographic permutation of the digits
-- 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

import           Data.List

-- We use a little permutation hack here, sort, and return the millionth entry
main = do
        let nums = permutations "0123456789"
        let ans = (sort nums) !! (10^6-1)
        writeFile "pe24.txt" ans
        print (read ans :: Integer)
