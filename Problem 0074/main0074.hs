{-
The number 145 is well known for the property that the sum of the factorial of its digits is equal to 145:

1! + 4! + 5! = 1 + 24 + 120 = 145

Perhaps less well known is 169, in that it produces the longest chain of numbers that link back to 169;
it turns out that there are only three such loops that exist:

169 -> 363601 -> 1454 -> 169
871 -> 45361 -> 871
872 -> 45362 -> 872

It is not difficult to prove that EVERY starting number will eventually get stuck in a loop. For example,

69 -> 363600 -> 1454 -> 169 -> 363601 -> (1454)
78 -> 45360 -> 871 -> 45361 -> (871)
540 -> 145 -> (145)

Starting with 69 produces a chain of five non-repeating terms, but the longest non-repeating chain
with a starting number below one million is sixty terms.

How many chains, with a starting number below one million, contain exactly sixty non-repeating terms?
-}

-- Memoization inspired by the post on p.2 of the PE 74 forum

import           Data.Array         as A
import           Data.Char
import           Data.IntMap.Strict as I
import qualified Data.List.Stream   as S
import           Data.Maybe

-- Create dictionary to reference how many non distinct terms remain after a certain term is seen
chainDict :: I.IntMap Int
chainDict = I.fromList [(871,1),(872,1),(45361,1),(45362,1),(169,2),(1454,2),(363601,2)]

-- Define a function to return the sum of the factorial of the digits
facts :: Array Int Int
sumFact :: Int -> Int
facts = array (0,9) [(0,1),(1,1),(2,2),(3,6),(4,24),(5,120),(6,720),(7,5040),(8,40320),(9,362880)]
sumFact n = S.sum $ S.map (\k -> facts A.! (digitToInt k)) (show n)

-- Given a mapping function number, return the length for any non-repeating chain
lenChain' :: (Int -> Int) -> Int -> Int
lenChain' g n
    | isJust (I.lookup n chainDict) = 1 + (fromJust $ I.lookup n chainDict)
    | n == n'    = 1
    | otherwise  = 1 + (g n')
    where 
        n' = sumFact n

-- Memoize the above function
memoize :: (Int,Int) -> ((Int -> Int) -> Int -> Int) -> (Int -> Int)
lenChain :: Int -> Int
memoize bounds f = arrayGet
                        where 
                            arr = array bounds [(j,f arrayGet j) | j<-range bounds]
                            arrayGet n = if inRange bounds n 
                                            then arr A.! n 
                                         else f arrayGet n
lenChain = memoize (1,100000) lenChain'

-- Print and write out the answer
main :: IO ()
main = do
        let ans = length $ S.filter (==60) [lenChain i| i<-[1..999999]]
        writeFile "pe74.txt" $ show ans
        print ans