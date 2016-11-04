@ECHO OFF
REM ###############################################
REM               fpcup for windows
REM             cross compile script.
REM ###############################################

ECHO.
ECHO ==============================================
ECHO   Build cross compiler for linux arm lamw
ECHO   (l)amw = android module wizard !!
ECHO ==============================================
ECHO.

if '%1'=='noconfirm' (
SET wait=--noconfirm
)

if EXIST .\fpcup.exe (
fpcup.exe --installdir="c:\prg" --ostarget="android" --cputarget="arm" --fpcOPT="-dFPC_ARMHF" --crossOPT="-CfSoft -CpARMV6" --only="FPCCleanOnly,FPCBuildOnly" --verbose %wait%
)

REM ###############################################
REM fpclazup can also be used
REM 
REM if EXIST .\fpclazup.exe (
REM fpclazup.exe --installdir="c:\prg" --ostarget="android" --cputarget="arm" --fpcOPT="-dFPC_ARMHF" --crossOPT="-CfSoft -CpARMV6" --
REM only="FPCCleanOnly,FPCBuildOnly" --verbose %wait%
REM )
REM 
REM ###############################################

ECHO.
ECHO ===============================================
ECHO   Build cross compiler for linux arm lamw ready
ECHO ===============================================
ECHO.
PAUSE