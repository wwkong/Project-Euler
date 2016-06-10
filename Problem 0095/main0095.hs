{-

The proper divisors of a number are all the divisors excluding the number itself. For example, the proper divisors of 28 are 1, 2, 4, 7, and 14. As the sum of these divisors is equal to 28, we call it a perfect number.

Interestingly the sum of the proper divisors of 220 is 284 and the sum of the proper divisors of 284 is 220, forming a chain of two numbers. For this reason, 220 and 284 are called an amicable pair.

Perhaps less well known are longer chains. For example, starting with 12496, we form a chain of five numbers:

12496 → 14288 → 15472 → 14536 → 14264 (→ 12496 → ...)

Since this chain returns to its starting point, it is called an amicable chain.

Find the smallest member of the longest amicable chain with no element exceeding one million.

-}

import Data.List
import Data.Maybe
import Data.Numbers.Primes
import qualified Data.Map as M
import Debug.Trace

-- Given a number, get the set of all divisors excluding the number itself
divisors :: Int -> [Int]
divisors n = init $ combinePs pFacts
    where 
        pFacts = group $ primeFactors n
        combinePs [] = [1]
        combinePs (lp@(p:ps):pss) = [(p^k)*rps | k <- [0..length lp], rps <- combinePs pss]

-- Get the amicable chain mapping for numbers 1<=k<=n with values (n0, l), smallest element and length of the chain
amiChain :: Int -> Int -> M.Map Int (Int, Int)
amiChain bound n = foldl addAmis M.empty [1..n]
    where 
        addAmis nm k
            | k  `M.member` nm || divs == [] = nm
            | k == sk                        = M.insert k (k,1)                     nm
            | otherwise                      = M.insert k (min k nextN, 1+nextL)    nm
            where 
                divs = divisors k
                sk = sum divs
                (nextN, nextL) = fromJust $ M.lookup k (addAmis nm sk)

-- Print and write
main :: IO()
main = do
        let ans  = 1
        writeFile "pe95.txt" $ show ans
        print ans