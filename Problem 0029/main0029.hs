-- How many distinct terms are in the sequence generated by
-- a^b for 2<=a<=100 and 2<=b<=100?

-- First application of functors and the Data.Set package

import           Control.Applicative
import           Data.Set

-- Print and write out the result
main :: IO()
main = do
        let perm = (^) <$> ([2..100] :: [Int]) <*> ([2..100] :: [Int])
        let ans = size $ fromList perm
        writeFile "pe29.txt" $ show ans
        print ans
