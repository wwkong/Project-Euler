{-

The proper divisors of a number are all the divisors excluding the number itself. For example, the proper divisors of 28 are 1, 2, 4, 7, and 14. As the sum of these divisors is equal to 28, we call it a perfect number.

Interestingly the sum of the proper divisors of 220 is 284 and the sum of the proper divisors of 284 is 220, forming a chain of two numbers. For this reason, 220 and 284 are called an amicable pair.

Perhaps less well known are longer chains. For example, starting with 12496, we form a chain of five numbers:

12496 → 14288 → 15472 → 14536 → 14264 (→ 12496 → ...)

Since this chain returns to its starting point, it is called an amicable chain.

Find the smallest member of the longest amicable chain with no element exceeding one million.

-}

import Data.List
import Data.Numbers.Primes
import Data.Maybe
import Debug.Trace

-- Given a number, get the set of all divisors excluding the number itself
divisors :: Int -> [Int]
divisors n = init $ combinePs pFacts
    where 
        pFacts = group $ primeFactors n
        combinePs [] = [1]
        combinePs (lp@(p:ps):pss) = [(p^k)*rps | k <- [0..length lp], rps <- combinePs pss]

-- Get the amicable chain of elments from a starting element
amiChain :: Int -> Int -> Maybe [Int]
amiChain bound n0 = amiChains' (Just [n0])
    where 
        amiChains' :: Maybe [Int] -> Maybe [Int]
        amiChains' mlst
            -- | trace ("n0="++show n0++", divs="++show divs++", sn="++show sn) False = undefined
            | divs == []                         = Nothing
            | sn > bound                         = Nothing
            | fromJust $ Just (elem sn) <*> mlst = pure (++) <*> Just [sn] <*> mlst
            | otherwise                          = amiChains' (pure (++) <*> Just [sn] <*> mlst)
            where 
                divs = divisors (head $ fromJust mlst)
                sn = sum divs

-- Get the set of amicable chains with (n,l) pairs of initial values and non-zero lengths of the chain
-- amiChains :: [(Int,Maybe Int)]
amiChains = [l | n <- [1..(10^6)], let l=amiChain (10^6) n, isJust l]

-- Print and write
main :: IO()
main = do
        let ans  = 1
        writeFile "pe95.txt" $ show ans
        print ans