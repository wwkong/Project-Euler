import Data.List
import Data.Bits (shiftL, shiftR) 

sqrtInt :: Integer -> Integer   
sqrtInt n = sqrtAlpha 1 where   
    sqrtAlpha i 
        | i * i > n = sqrtBeta (shiftR i 1) 0   
        | otherwise = sqrtAlpha (shiftL i 1)    
    sqrtBeta 0 acc = acc    
    sqrtBeta i acc = sqrtBeta (div i 2) (if (i + acc)^2 <= n then i + acc else acc) 

continuedFractionSqrt s = let rs=sqrtInt s in rs:(continuedFractionSqrtHelp s (rs,0,1,rs))
continuedFractionSqrtHelp s (aold,mold,dold,rs) = anew:(continuedFractionSqrtHelp s (anew,mnew,dnew,rs))
    where 
        mnew=dold*aold - mold
        dnew=div (s-mnew*mnew) dold
        anew=(div (rs+mnew) dnew)

convergents' cf 0 =[(cf!!0,1)]
convergents' cf 1 =[((cf!!0)*(cf!!1)+1,(cf!!1)),(cf!!0,1)]
convergents' cf n =((cf!!n)*h1+h2 ,(cf!!n)*k1+k2):(h1,k1):(h2,k2):cvts
        where (h1,k1):(h2,k2):cvts = (convergents' cf (n-1))

convergents cf = (cf!!0,1):((cf!!0)*(cf!!1)+1,(cf!!1)):[((cf!!n)*h1+h2 ,(cf!!n)*k1+k2)|n<-[2..], let (h1,k1)= (convergents cf)!!(n-1), let (h2,k2)= (convergents cf)!!(n-2)]

convergents10ofsqrt3 = reverse $ convergents' (continuedFractionSqrt 3) 10

firstPell3= head (filter (\(h,k)->h*h-3*k*k==1) convergents10ofsqrt3)

pell3list = firstPell3:[(x0*xn+3*y0*yn,x0*yn+y0*xn)|n<-[0..], let (x0,y0)=firstPell3, let (xn,yn)=pell3list!!n  ]

pell3list' 0 = [firstPell3]
pell3list' n =(x0*xn+3*y0*yn,x0*yn+y0*xn):(xn, yn):xys
        where 
            (xn, yn):xys = pell3list' (n-1)
            (x0,y0) = firstPell3

--pell3list 

pellList = takeWhile (\(x,y)-> x <(div (10^9+8) 2)) $reverse $ pell3list' 100

f x
    |x `mod` 3 == 1 =2*x+2
    |x `mod` 3 == 2 =2*x-2
    |otherwise =0

main = do print $ sum $map (\(x,y) -> f x) $tail pellList
--main = do print pellList