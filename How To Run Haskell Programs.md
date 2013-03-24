-- GHCI Shell
* Simply call the main function using "> main" and if you want
* To see performance you can run the "> :set +s" command and then call the program again

-- GHC (In Windows Shell)
* Compile the program using "ghc -O main.hs -rtsopts" in the Windows shell where "main.hs" is your program's name
	\-> Here, -O is the optimization flag
* To see performance, use "main +RTS -sstats.txt -RTS" where "main" is the name of your program "main.hs"
	\-> A file called "stats.txt" will be placed in the same directory (-s was the RTS flag)
	\-> You can add the -K512M flag to allocate 512MB to the heap (other sizes can be specified)
