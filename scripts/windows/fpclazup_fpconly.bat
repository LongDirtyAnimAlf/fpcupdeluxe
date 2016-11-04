@ECHO OFF
REM #####################################################
REM               fpclazup for windows
REM #####################################################

ECHO.
ECHO ====================================================
ECHO   Fpclazup default; fpc only
ECHO ====================================================
ECHO.

if EXIST .\fpclazup.exe (
fpclazup.exe --only="fpc,FPCCrossWin32-64" --verbose
)

ECHO.
ECHO ====================================================
ECHO   Fpclazup default ready; fpc only
ECHO ====================================================
ECHO.
PAUSE