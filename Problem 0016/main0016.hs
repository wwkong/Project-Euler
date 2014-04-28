-- What is the sum of the digits of the number 2^1000?

import           Data.Char

-- Print and write the
main = do
        let ans = sum $ map digitToInt $ show (2 ^ 1000)
        writeFile "pe16.txt" $ show ans
        print ans
