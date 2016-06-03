binIntSearchHelper1 :: (Integral a0)=>(a0->Bool)->a0
binIntSearchHelper1 proposition = head [k | k<-[0..], proposition (2^k)]

binIntSearchHelper2 :: (Integral a0)=>(a0 ->Bool)->(a0)->(a0)->(a0)
binIntSearchHelper2 proposition l u
  |l == u = l
  |l+1 == u = u
  |proposition midpt = binIntSearchHelper2 proposition l midpt
  |otherwise = binIntSearchHelper2 proposition midpt u
  where midpt = l+(div (u-l) 2)

binIntSearch :: (Integral a0)=>(a0->Bool)->a0
binIntSearch proposition = binIntSearchHelper2 proposition l (2*l)
  where l = 2^((binIntSearchHelper1 proposition)-1)
