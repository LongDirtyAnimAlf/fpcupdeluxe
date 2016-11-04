@ECHO OFF
REM ###############################################
REM               fpclazup for windows
REM ###############################################

ECHO.
ECHO ==============================================
ECHO   Fpclazup with trunk and RTTI patch
ECHO ==============================================
ECHO.

if EXIST .\fpclazup.exe (
fpclazup.exe --fpcURL="trunk" --lazURL="trunk" --fpcPATCH="fpctrunkrtti.patch"
)

ECHO.
ECHO ==============================================
ECHO   Fpclazup with trunk and RTTI patch ready
ECHO ==============================================
ECHO.
PAUSE
