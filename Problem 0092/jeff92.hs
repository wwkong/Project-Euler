
import Data.List

diglist n
                |n<10 = [n]
                |otherwise = (mod n 10):(diglist (div n 10))


myFun loi = diglist $ foldr (\x y ->sqr(x)+y) 0 loi

sqr x
  |x==0 =0
  |x==1 =1
  |x==2 =4
  |x==3 =9
  |x==4 =16
  |x==5 =25
  |x==6 =36
  |x==7 =49
  |x==8 =64
  |x==9 =81



factorial::Int->Int
factorial n 
                |n==1 =1
                |n==2 =2
                |n==3 =6
                |n==4 =24
                |n==5 =120
                |n==6 =720
                |n==7 =5040
                |otherwise = n*(factorial (n-1)  )

ans::Int
ans  = sum [if ((myFun2 l) == 0) then 0 else (div (factorial 7) (product $ map (\x-> factorial$length x) $ group l)) | 
                m1<-[1..9],
                m2<-[0..m1],
                m3<-[0..m2],
                m4<-[0..m3],
                m5<-[0..m4],
                m6<-[0..m5],
                m7<-[0..m6],
                let l=[m1,m2,m3,m4,m5,m6,m7]]


myFun2::[Int]->Int
myFun2 loi
                | loi==[1] = 0
                | loi==[9,8] = 1
                | otherwise =myFun2 $ myFun loi


--test = foldr (\x y->(myFun2 x) + y) 0 (map diglist [1..10000000])


main = do
                print ans
