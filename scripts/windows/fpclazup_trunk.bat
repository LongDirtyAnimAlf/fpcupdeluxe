@ECHO OFF
REM ###############################################
REM               fpclazup for windows
REM ###############################################

ECHO.
ECHO ==============================================
ECHO   Fpclazup with trunk and defaults
ECHO ==============================================
ECHO.

if EXIST .\fpclazup.exe (
fpclazup.exe --fpcURL="trunk" --lazURL="trunk" --verbose
)

ECHO.
ECHO ==============================================
ECHO   Fpclazup with trunk and defaults ready
ECHO ==============================================
ECHO.
PAUSE