@ECHO OFF
SETLOCAL EnableDelayedExpansion
FOR /R %%f IN (*.hstats) DO (
	SET prog=%%~nf
	SET progNum=!prog:stats=!
	IF NOT EXIST %~dp0Statistics\%%~nxf (
	ECHO Copying statistics for Project Euler Problem !progNum! to:
	xcopy /Y "%%~dpnxf" "%~dp0\Statistics"
	)
)
pause 