@ECHO OFF
REM ###############################################
REM               fpcup for windows
REM             cross compile script.
REM ###############################################

ECHO.
ECHO ==============================================
ECHO   Build cross compiler for linux arm hardfloat
ECHO ==============================================
ECHO.

if '%1'=='noconfirm' (
SET wait=--noconfirm
)

if EXIST .\fpcup.exe (
fpcup.exe --ostarget="linux" --cputarget="arm" --fpcOPT="-dFPC_ARMHF" --crossOPT="-CpARMV7A -CfVFPV3 -OoFASTMATH -CaEABIHF" --only="FPCCleanOnly,FPCBuildOnly" %wait%
)

REM ###############################################
REM fpclazup can also be used
REM 
REM if EXIST .\fpclazup.exe (
REM fpclazup.exe --ostarget="linux" --cputarget="arm" --fpcOPT="-dFPC_ARMHF" --crossOPT="-CpARMV7A -CfVFPV3 -OoFASTMATH -CaEABIHF" --
REM only="FPCCleanOnly,FPCBuildOnly" %wait%
REM )
REM 
REM ###############################################

ECHO.
ECHO ==============================================
ECHO   Build cross compiler for linux arm ready
ECHO ==============================================
ECHO.
PAUSE