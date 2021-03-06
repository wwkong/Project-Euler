{-

A natural number, N, that can be written as the sum and product of a given set of at least two natural numbers, {a1, a2, ... , ak} is called a product-sum number: N = a1 + a2 + ... + ak = a1 × a2 × ... × ak.

For example, 6 = 1 + 2 + 3 = 1 × 2 × 3.

For a given set of size, k, we shall call the smallest N with this property a minimal product-sum number. The minimal product-sum numbers for sets of size, k = 2, 3, 4, 5, and 6 are as follows.

k=2: 4 = 2 × 2 = 2 + 2
k=3: 6 = 1 × 2 × 3 = 1 + 2 + 3
k=4: 8 = 1 × 1 × 2 × 4 = 1 + 1 + 2 + 4
k=5: 8 = 1 × 1 × 2 × 2 × 2 = 1 + 1 + 2 + 2 + 2
k=6: 12 = 1 × 1 × 1 × 1 × 2 × 6 = 1 + 1 + 1 + 1 + 2 + 6

Hence for 2≤k≤6, the sum of all the minimal product-sum numbers is 4+6+8+12 = 30; note that 8 is only counted once in the sum.

In fact, as the complete set of minimal product-sum numbers for 2≤k≤12 is {4, 6, 8, 12, 15, 16}, the sum is 61.

What is the sum of all the minimal product-sum numbers for 2≤k≤12000?

-}

import           Data.List.Extra
import           Data.List
import           Data.Numbers.Primes (primeFactors, isPrime)
import qualified Data.Map as M 

-- Takes in a finite repeating list of primes and returns the set of unique factorizations efficiently 
ppFactorize :: [Int] -> [[Int]]
ppFactorize [] = [[]]
ppFactorize ps = ppFactorize' e0 e0
    where 
        (p, e0) = (head ps, length ps)
        ppFactorize' mx e
            | e == 0    = [[]]
            | mx == 1   = [take e (repeat p)]
            | otherwise = [(take m (repeat (p^k))) ++ sfs | k <- [1..min mx e], 
                                                            let lowerM = if k/=1 then 1 else e,
                                                            let upperM = if k/=1 then (div e k) else e, 
                                                            m <- [lowerM..upperM], 
                                                            sfs <- if k==1 then [[]] else ppFactorize' (k-1) (e-m*k)]

-- > ---- INSERT JEFFREY NEGREA'S CODE HERE ---- <

partitions2ff :: [(Int,Int)]->[(Int,Int)]->[[(Int,Int)]]
partitions2ff _ [] =[]
partitions2ff [] _ =[]
partitions2ff [(ma,a)] [(mb,b)] = [ if k==0 then [(ma,a),(mb,b)]
                                    else if pa && pb then [(k,a*b)]
                                    else if pa then [(mb',b),(k,a*b)]
                                    else if pb then [(ma',a),(k,a*b)]
                                    else [(ma',a),(mb',b),(k,a*b)] | 
                                        k<-[0..(min ma mb)], 
                                        let ma'=ma-k, 
                                        let mb'=mb-k, 
                                        let pa=(ma'==0),
                                        let pb=(mb'==0)]

partitions2ff [(ma,a)] ((mb,b):mbbs) = [    if k==0 then (mb,b):l
                                    else if pa && pb then (k,a*b):mbbs
                                    else if pa then (mb',b):(k,a*b):mbbs
                                    else if pb then (k,a*b):l
                                    else (mb',b):(k,a*b):l| 
                                        k<-[0..(min ma mb)], 
                                        let ma'=ma-k, 
                                        let mb'=mb-k, 
                                        let pa=(ma'==0),
                                        let pb=(mb'==0),
                                        l<- (if (not ((k==0) || (not pa))) then [[]] else (partitions2ff [(ma',a)] mbbs))]

partitions2ff ((mb,b):mbbs) [(ma,a)] =  partitions2ff [(ma,a)] ((mb,b):mbbs)

partitions2ff ((ma,a):maas) mbbs = [if ll==[] then (ma,a):l2 else l1++l2|
        l<-(partitions2ff maas mbbs),
        let ll = (partitions2ff [(ma,a)] (filter (\(x,y)->(gcd a y) ==1) l)),
        let ll' = if ll==[] then [[]] else ll,
        l1<-ll',
        let l2=(filter (\(x,y)->(gcd a y) >1) l)]

partitions2ll :: [[(Int,Int)]]->[[(Int,Int)]]->[[(Int,Int)]]
partitions2ll l1 l2 = concat [partitions2ff maas baas|maas<-l1, baas<-l2]

-- > ---- END JEFFREY NEGREA'S CODE HERE ---- <

-- Get the list of disjoint factorizations of prime groups
ppConvert :: Int -> [[(Int,Int)]]
ppConvert n = foldl1 partitions2ll disjointF
    where 
        disjointF = map (convert . ppFactorize) $ group (primeFactors n)
        convert lofe = map ((map (\l -> (length l, head l))) . group) lofe        

-- Adapt Jeff's version of the score function
score :: Int -> [(Int,Int)]
score n = sort [(k,n) | f <- ppConvert n, let k = calcK f, k/=1]
    where
        calcK lofe = n - (foldr (\(a,b) n-> n+a*(b-1)) 0 lofe)

-- Calculate the sum of the minimum product numbers k for 2≤k≤n
sumMinProdSum n = sumDistinct $ foldl' addKs M.empty . filter (not . isPrime) $ [2..(13*n) `div` 12]
    where 
        foldl' f z []       =   z
        foldl' f z (x:xs)   =   let z' = z `f` x in seq z' $ foldl' f z' xs
        sumDistinct         =   sum . nubOrd . M.elems
        addKs nm m          =   foldl' (addK m) nm (score m)
        addK m nm (ki, ni)
            | ki > n || ki `M.member` nm    = nm
            | otherwise                     = M.insert ki ni nm

-- Print and write
main :: IO()
main = do
        let ans  = sumMinProdSum 12000
        writeFile "pe88.txt" $ show ans
        print ans