-- Will be revisited because I will be doing this by hand

-- Given that the three characters are always asked for in order, analyse the file keylog.txt so as to
-- determine the shortest possible secret passcode of unknown length.

-- Note that the shortest length will have only one occurence of each digit in the set of digits

import           Data.Char
import           Data.List
import qualified Data.Set    as S

-- Given a set of numbers as strings, create a function to find the set of unique digits
uniqueDigits :: [String] -> [Int]
uniqueDigits lst = (S.toList . S.fromList) $ map digitToInt $ concat lst

-- Create a function that takes in a list of numbers, as strings, and a list of digits
-- and returns the digit that exists in the head of each element in the list but nowhere else
uniqueHeadDigits :: [Int] -> [String] -> [Int]
uniqueHeadDigits digits numLst =    [n | n <- digits,
                                    (n `elem` (map digitToInt $ map head numLst)) &&
                                    (not $ n `elem` (uniqueDigits $ map tail numLst))]

-- Create a function that solves our problem through wrapping the above function
findPass :: [Int] -> [String] -> [Int]
findPass digits numLst
    | digits == [] = []
    | curDigit == [] = findPass digits (map tail numLst)
    | otherwise = curDigit ++ (findPass (digits \\ curDigit) modLst)
    where
        curDigit = uniqueHeadDigits digits numLst
        modLst = map (\nStr -> if length nStr == 1 then nStr else nStr \\ (show curDigit)) numLst

-- Print and write out the answer
main :: IO()
main = do
        rawData <- readFile "keylog.txt"
        let contents = (S.toList . S.fromList . lines) rawData -- Remove duplicates
        let ans = read $ map intToDigit $ findPass (uniqueDigits contents) contents :: Integer
        writeFile "pe79.txt" $ show ans
        print ans

