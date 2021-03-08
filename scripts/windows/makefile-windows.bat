@if "%DEBUG%" == "" @echo off
@rem ##########################################################################
@rem
@rem  Regenerate makefile buildscript for Windows
@rem
@rem ##########################################################################

@rem Set local scope for the variables with windows NT shell
if "%OS%"=="Windows_NT" setlocal

@rem set FPC=C:\fpclazbydeluxe\trunklatest2\fpc\bin\i386-win32
set FPC=C:\fpclazbydeluxe\trunklatest2\fpcscr
set FPCDIR=C:\fpclazbydeluxe\trunklatest2\fpcscr
@rem set LAZARUS=C:\fpclazbydeluxe\trunklatest2\lazarus
set LAZARUS=C:\Users\Alfred\Documents\GitHub\lazarus

set path=C:\fpclazbydeluxe\trunklatest2\fpc\bin\i386-win32;%path%
@rem set path=C:\fpclazbydeluxe\trunklatest2\fpc;%path%
@rem set path=C:\fpclazbydeluxe\trunklatest2\fpcscr;%path%

@rem call %LAZARUS%\project1.exe

call %LAZARUS%\fpcmake.exe -Tall -v -r -w %LAZARUS%\ide\Makefile.fpc

if "%OS%"=="Windows_NT" endlocal

pause