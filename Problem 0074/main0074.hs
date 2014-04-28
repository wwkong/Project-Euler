{-
The number 145 is well known for the property that the sum of the factorial of its digits is equal to 145:

1! + 4! + 5! = 1 + 24 + 120 = 145

Perhaps less well known is 169, in that it produces the longest chain of numbers that link back to 169;
it turns out that there are only three such loops that exist:

169 -> 363601 -> 1454 -> 169
871 -> 45361 -> 871
872 -> 45362 -> 872

It is not difficult to prove that EVERY starting number will eventually get stuck in a loop. For example,

69 -> 363600 -> 1454 -> 169 -> 363601 -> ( 1454)
78 -> 45360 -> 871 -> 45361 -> (871)
540 -> 145 -> (145)

Starting with 69 produces a chain of five non-repeating terms, but the longest non-repeating chain
with a starting number below one million is sixty terms.

How many chains, with a starting number below one million, contain exactly sixty non-repeating terms?
-}

import           Data.Char
import           Data.Maybe
import qualified Data.MemoCombinators as Memo

-- Create dictionary to reference how many non distinct terms remain after a certain term is seen
chainDict = [(871,1),(872,1),(45361,1),(45362,1),(169,2),(1454,2),(363601,2)]

-- Define a function to return the sum of the factorial of the digits
facts = [(0,1),(1,1),(2,2),(3,6),(4,24),(5,120),(6,720),(7,5040),(8,40320),(9,362880)]
factorial = fromJust . ((flip lookup) facts)
sumFact n = sum $ map (factorial . digitToInt) (show n)

-- Given a number, return length of the non-repeating chain
chainLen k = (Memo.integral chainLen') k where
    chainLen' k
        | k == next = 1
        | ref /= Nothing = 1 + (fromJust ref)
        | otherwise = 1 + chainLen' next
        where
            ref = lookup k chainDict
            next = sumFact k

-- Print and write out the answer
main = do
        let ans = length [n | k <- [1..10^6], let n = (Memo.integral chainLen) k, n == 60]
        writeFile "pe74.txt" $ show ans
        print ans
