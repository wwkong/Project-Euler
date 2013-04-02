@ECHO OFF
SETLOCAL EnableDelayedExpansion

FOR /R %%f IN (main*.hs) DO (
	ECHO ----------------------------
	SET progPath=%%~dpf
	CD !progPath!
	SET prog=%%~nf
	SET progNum=!prog:main=!
	ghc -O %%f -rtsopts
	ECHO ----------------------------
	ECHO Project Euler !progNum! has been compiled with output:
	!prog! +RTS -sstats!progNum!.hstats -K512M -RTS
	CD .
)
pause