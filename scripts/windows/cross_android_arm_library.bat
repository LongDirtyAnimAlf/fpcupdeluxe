@ECHO OFF
REM ###############################################
REM               fpcup for windows
REM             cross compile script.
REM ###############################################

ECHO.
ECHO ==============================================
ECHO   Build cross compiler for android arm with path
ECHO ==============================================
ECHO.

if '%1'=='noconfirm' (
SET wait=--noconfirm
)

if EXIST .\fpcup.exe (
fpcup.exe --ostarget="android" --cputarget="arm" --only="FPCCleanOnly,FPCBuildOnly" --crossbindir=C:\Users\Me\Downloads\android-ndk-r10e\toolchains\arm-linux-androideabi-4.9\prebuilt\windows\bin --crosslibdir=C:\Users\Me\Downloads\android-ndk-r10e\platforms\android-21\arch-arm\usr\lib %wait%
)

REM Delphi auto-installed toolchains can also be used !
REM --crossbindir=C:\Users\Public\Documents\Embarcadero\Studio\17.0\PlatformSDKs\android-ndk-r9c\toolchains\arm-linux-androideabi-4.9\prebuilt\windows\bin
REM --crosslibdir=C:\Users\Public\Documents\Embarcadero\Studio\17.0\PlatformSDKs\android-ndk-r9c\platforms\android-21\arch-arm\usr\lib

ECHO.
ECHO ==============================================
ECHO   Build cross compiler for android arm ready
ECHO ==============================================
ECHO.
PAUSE