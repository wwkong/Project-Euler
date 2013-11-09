main = putStrLn (show (foo (2^20) (0,0)))
foo end (i,acc)
	| i == end = acc
	| otherwise = foo end $! (i+1,acc+1)