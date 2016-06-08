-- From Jeffrey Negrea

import Data.List

partitions2pp::(Int,Int)->Int->[[(Int,Int)]]
partitions2pp (p,0) _ = [[]]
partitions2pp (p,e) 1 = [[(e,p)]]
partitions2pp (p,e) maxOrder = [(m,p^k):x|
    k<-[1..(min e maxOrder)],
    let lowerM = if k/=1 then 1 else e,
    m<-[lowerM..(div e k)],
    x <-  if k==1 then [[]] else (partitions2pp (p,e-m*k) (k-1))]  

partitions2pp'::(Int,Int)->[[(Int,Int)]]
partitions2pp' (p,e) = partitions2pp (p,e) e

partitions2ff:: [(Int,Int)]->[(Int,Int)]->[[(Int,Int)]]
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
        let l2=(filter (\(x,y)->(gcd a y) >1) l)
        ]

partitions2ll:: [[(Int,Int)]]->[[(Int,Int)]]->[[(Int,Int)]]
partitions2ll l1 l2 = concat [partitions2ff maas baas|maas<-l1, baas<-l2]

partitions:: [(Int,Int)]->[[(Int,Int)]]
partitions [(p,e)] = partitions2pp' (p,e)
partitions lope = foldr1 partitions2ll $ map partitions2pp' lope            

partitionsUloPE::(Int,[(Int,Int)])->[(Int,[(Int,Int)])]
partitionsUloPE (u,loPE) = [(u,l)|l<-(partitions loPE)]

score :: (Int,[(Int,Int)]) -> (Int,Int)
score (u,loPE) = (u - (foldr (\(a,b) n-> n+a*(b-1)) 0 loPE),u)

partitionsClean::[(Int,Int)]->[[Int]]
partitionsClean lope= map (\ll->sort $ foldr (\(a,b) l-> (take a $ repeat b)++l) [] ll) $  partitions lope

squareRoot :: Int -> Int
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^2 <= n && n < (r+1)^2
  in  head $ dropWhile (not . isRoot) iters

primesHelper:: [Int]-> Int-> Int ->[Int]
primesHelper [] m sqrtM =[]
primesHelper [a] m sqrtM =[a]
primesHelper (a:xs) m sqrtM
    |a>(sqrtM+1) =a:xs
    |otherwise =a:(primesHelper (filter (\x-> mod x a /=0) xs) m sqrtM)

primes::Int -> [Int]
primes m = (primesHelper (2:[3,5..m]) m (squareRoot m))

unfactorized:: [(Int,Int)]->Int
unfactorized lopk = product [p^k|(p,k)<-lopk]

integerLog :: Int->Int->Int
integerLog n p = head [k | k<-[0..], p^k > n]

loIntsHelper::[Int]->[(Int,[(Int,Int)])]->Int->[(Int,[(Int,Int)])]
loIntsHelper [] ilist max = ilist
loIntsHelper (p:plist) ilist mx = 
  (filter (\x-> ((div mx (fst x)) < p)) ilist)++(loIntsHelper plist 
    ([(u, mynum)
    |i<-(filter (\x-> ((div mx (fst x)) >= p)) ilist),
    k<-[1..((integerLog (div mx (fst i)) p) -1)],
    let mynum= filter (/=(1,1)) $ sort ((p,k):(snd i)),
    let u = (fst i)*p^k
    ] ++(filter (\x-> ((div mx (fst x)) >= p)) ilist)) mx)

loInts::Int->[(Int,[(Int,Int)])]
loInts n = sort $ loIntsHelper (primes n) [(1,[(1,1)])] n

lEqFold:: [(Int,Int)]->(Int,Int)->[(Int,Int)]
lEqFold [] (a,b) = [(a,b)]
lEqFold ((p,q):loab) (a,b)
    |(a==p) = ((p,q):loab)
    |otherwise =(a,b):((p,q):loab)

rAdd::Int->(Int,Int)->Int
rAdd n (a,b) = b+n

myWrap m = foldl rAdd 0 $ map (\(a,b)->(b,a))$ foldl lEqFold [] $sort $ map (\(a,b)->(b,a)) $foldl lEqFold [] $ sort $filter (\(s,u)-> s<=m && s>1) $ map score $ concat $ map partitionsUloPE (loInts (div (13*m) 12) )

main :: IO()
main = do 
    print (myWrap 12000)
    --print test1
    --print test2