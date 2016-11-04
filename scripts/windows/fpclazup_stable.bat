@ECHO OFF
REM ###############################################
REM               fpclazup for windows
REM ###############################################

ECHO.
ECHO ==============================================
ECHO   Fpclazup stable and defaults
ECHO ==============================================
ECHO.

if EXIST .\fpclazup.exe (
fpclazup.exe --fpcURL="stable" --lazURL="stable" --verbose
)

ECHO.
ECHO ==============================================
ECHO   Fpclazup stable and defaults ready
ECHO ==============================================
ECHO.
PAUSE