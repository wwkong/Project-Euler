@ECHO OFF
SETLOCAL EnableDelayedExpansion

FOR /R %%f IN (main*.hs) DO (
	SET progPath=%%~dpf
	CD !progPath!
	SET prog=%%~nf
	SET progHaskell=%%f
	SET progExe=!progHaskell:.hs=.exe!
	SET progNum=!prog:main=!
	IF NOT EXIST "!progExe!" (
	ghc -O "%%f" -rtsopts
	ECHO ---------------------------------------------------------------------------
	ECHO Project Euler !progNum! has been compiled.
	ECHO ---------------------------------------------------------------------------
	)
	IF NOT EXIST !progPath!stats!progNum!.hstats (
	ECHO ---------------------------------------------------------------------------
	ECHO Project Euler !progNum! has been executed with output:
	!prog! +RTS -sstats!progNum!.hstats -K512M -RTS
	ECHO A statistics file has been generated with name: stats!progNum!.hstats.
	ECHO ---------------------------------------------------------------------------

	)
	CD .
)
pause