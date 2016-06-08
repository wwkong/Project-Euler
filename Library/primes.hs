-- Prime Sieve from Jeffrey Negrea

import Data.List 

squareRoot :: Int -> Int
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r = r^2 <= n && n < (r+1)^2
  in head $ dropWhile (not . isRoot) iters

primesHelper [] m sqrtM =[]
primesHelper [a] m sqrtM =[a]
primesHelper (a:xs) m sqrtM
  |a>(sqrtM+1) =a:xs
  |otherwise =a:(primesHelper (filter (\x-> mod x a /=0) xs) m sqrtM)

primes m = (primesHelper (2:[3,5..m]) m (squareRoot m))

-- Given an integer n, find the set of all factors of n
factorsN :: Int -> [Int]
factorsN n =  sort $ foldl (\l1 l2 -> [a*b|a<-l1,b<-l2]) [1] pMult
    where 
        pFact = group (primeFactors n)
        pDecmp = zip (map head pFact) (map length pFact)
        pMult = map (\(p,e) -> [p^k | k <- [0..e]]) pDecmp

-- Takes in a finite repeating list of primes and returns the set of unique factorizations efficiently 
ppFactorize :: [Int] -> [[Int]]
ppFactorize [] = [[]]
ppFactorize ps = ppFactorize' e0 e0
    where 
        (p, e0) = (head ps, length ps)
        ppFactorize' mx e
            -- | trace ("\nCalled with mx=" ++ show mx ++ ", e=" ++ show e ++ "\n") False = undefined
            | mx == 1   = [take e (repeat p)]
            | otherwise = [(take m (repeat (p^k))) ++ sfs | k <- [1..min mx e], 
                                                            let lowerM = if k/=1 then 1 else e,
                                                            let upperM = if k/=1 then (div e k) else e, 
                                                            m <- [lowerM..upperM], 
                                                            sfs <- if k==1 then [[]] else ppFactorize' (k-1) (e-m*k)]

-- Take two pairwise relatively prime factor lists and get the unique set of factor combinations
ppsFactorize :: [Int] -> [Int] -> [[Int]]
ppsFactorize fs1 fs2 
    | l1 <= 0 || l2 <= 0    = []
    | l1 == 1 && l2 == 1    = [fs1 ++ fs2]
    | l1 == 1               = [fs1 ++ fs2] ++ [[f1 * (fst rms)] ++ snd rms | rms <- rmOneElem fs2]
    | l2 == 1               = [fs1 ++ fs2] ++ [[f2 * (fst rms)] ++ snd rms | rms <- rmOneElem fs1]
    | otherwise             = [fs1 ++ fs2] ++ [((fst rms1) * (fst rms2)) : next | 
                                                rms1 <- rmOneElem fs1, rms2 <- rmOneElem fs2,
                                                next <- ppsFactorize (snd rms1) (snd rms2)]
    where 
        (l1, l2, f1, f2, tfs1, tfs2) = (length fs1, length fs2, head fs1, head fs2, tail fs1, tail fs2)
        rmOneElem [x] = [(x,[])]
        rmOneElem xs = [(head rxs, hxs ++ tail rxs) |   n <- [0..(length xs)-1], 
                                                        let hxs = take n xs, 
                                                        let rxs = drop n xs]