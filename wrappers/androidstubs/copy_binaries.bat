@echo off

set SOURCE=..\..\..\..\mingw64\bin
set TARGET=..\target\bin

CALL :NORMALIZEPATH "%SOURCE%"
SET SOURCE=%RETVAL%

CALL :NORMALIZEPATH "%TARGET%"
SET TARGET=%RETVAL%

set list=clang.exe clang++.exe dsymutil.exe llvm-strip.exe strip.exe libclang-cpp.dll libclang.dll libdl.dll libffi-8.dll libgcc_s_seh-1.dll libiconv-2.dll libLLVM-15.dll libLLVM-16.dll liblzma-5.dll libstdc++-6.dll libwinpthread-1.dll libxml2-2.dll libzstd.dll zlib1.dll

(for %%a in (%list%) do (
   copy "%SOURCE%\%%a" "%TARGET%\%%a"
))

echo "Done !"

:: ========== FUNCTIONS ==========
EXIT /B

:NORMALIZEPATH
  SET RETVAL=%~f1
  EXIT /B
