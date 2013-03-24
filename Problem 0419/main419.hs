-- Incomplete

import Data.Maybe

-- Define a dictionary
lookDict = [("1",("11",(1,0,0))), ("11",("21",(-1,1,0))), ("111",("31",(-2,0,1))),
			("2",("12",(1,0,0))), ("22",("22",(0,0,0))),  ("222",("32",(0,-2,1))),
			("3",("13",(1,0,0))), ("33",("23",(0,1,-1))), ("333",("33",(0,0,-1)))]
			
-- Implement add tuple
addT (a,b,c) (d,e,f) = (a+d,b+e,c+f)

parseChange :: String -> (Int,Int,Int) -> (String, (Int,Int,Int))
parseChange s t = (fst $ fromJust $ lookup s lookDict, addT (snd $ fromJust $ lookup s lookDict) t)

--lookAndSay n = lookAndSay' "1" (0,0,0) where

main = return $ parseChange "333" (0,0,0)