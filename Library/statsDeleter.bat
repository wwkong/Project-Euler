@ECHO OFF
SETLOCAL EnableDelayedExpansion

FOR /R %%f IN (main*.hs) DO (
	SET progPath=%%~dpf
	CD !progPath!
	SET prog=%%~nf
	SET progHaskell=%%f
	SET progExe=!progHaskell:.hs=.exe!
	SET progNum=!prog:main=!
	IF EXIST "!progExe!" (
	DEL "!progExe!"
	ECHO ---------------------------------------------------------------------------
	ECHO The executable for Project Euler !progNum! has been deleted.
	ECHO ---------------------------------------------------------------------------
	)
	IF EXIST "!progPath!stats!progNum!.hstats" (
	DEL "!progPath!stats!progNum!.hstats"
	ECHO ---------------------------------------------------------------------------
	ECHO The stats for Project Euler !progNum! have been deleted.
	ECHO ---------------------------------------------------------------------------

	)
	CD .
)
pause