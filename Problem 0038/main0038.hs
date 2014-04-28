{-
What is the largest 1 to 9 pandigital 9-digit number that can be formed
as the concatenated product of an integer with (1,2, ... , n) where n > 1?

Example : 9 and (1,2,3,4,5) = 918273645, 192 and (1,2,3) = 192384576
-}

import           Data.List
import qualified Data.Set  as S

-- First note that the first digit must start we a 9 to be larger than the
-- given example with a limit of 9876

-- We create a list of these possible numbers
panGen n = map (read :: String -> Integer) $ (S.toList . S.fromList) [take n s | s <- permutations "123456789", head s == '9']
panNums = [9] ++ (panGen 2) ++ (panGen 3) ++ (panGen 4)

-- For a given number define a function to return 0 if there exists no n to
-- create a pandigital concatenated product and the pandigital number otherwise
panCProd n = panCProd' n 1 (show n) where
    panCProd' n curPosn panStr
        | totLen > 9 = 0
        | totLen == 9 =
            if (sort panStr) == "123456789" then read panStr :: Integer else 0
        | otherwise = panCProd' n (curPosn + 1) (panStr ++ nextStr)
        where
            totLen = length panStr
            nextStr = show $ n * (curPosn + 1)

-- Print and write out the answer
main = do
        let ans = maximum $ map panCProd panNums
        writeFile "pe38.txt" $ show ans
        print ans
