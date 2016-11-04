@ECHO OFF
REM ###############################################
REM               fplazcup for windows
REM             cross compile script.
REM ###############################################

ECHO.
ECHO ===============================================
ECHO   Build cross compiler for windows 64 bit
ECHO ===============================================
ECHO.

if '%1'=='noconfirm' (
SET wait=--noconfirm
)

if EXIST .\fpclazup.exe (
fpclazup.exe --ostarget="win64" --cputarget="x86_64" --only="FPCCleanOnly,FPCBuildOnly" --verbose %wait%
)

ECHO.
ECHO ===============================================
ECHO   Build cross compiler for windows 64 bit ready
ECHO ===============================================
ECHO.
PAUSE