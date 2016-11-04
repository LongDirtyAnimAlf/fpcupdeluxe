@ECHO OFF
REM #####################################################
REM               fpclazup for windows
REM #####################################################

ECHO.
ECHO ====================================================
ECHO   Fpclazup default; Lazarus only
ECHO ====================================================
ECHO.

if EXIST .\fpclazup.exe (
fpclazup.exe --lazURL="default" --skip="fpc,FPCCrossWin32-64" --verbose
)

ECHO.
ECHO ====================================================
ECHO   Fpclazup default ready; Lazarus only
ECHO ====================================================
ECHO.
PAUSE