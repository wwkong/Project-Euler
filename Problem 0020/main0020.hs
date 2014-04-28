-- Find the sum of the digits in the number 100!

import           Data.Char

-- Print and write out the answer
main = do
        let ans = sum $ map digitToInt $ show $ product [1..100]
        writeFile "pe20.txt" $ show ans
        print ans
