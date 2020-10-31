@if "%DEBUG%" == "" @echo off
@rem ##########################################################################
@rem
@rem  Android libiconv buildscript for Windows
@rem  Please set the path towards the NDK
@rem
@rem ##########################################################################

@rem Set local scope for the variables with windows NT shell
if "%OS%"=="Windows_NT" setlocal

set java=C:\Program Files\Java\jdk1.8.0_271\bin

set javac=%java%\javac.exe
set jar=%java%\jar.exe

call "%javac%" net\sourceforge\zbar\*.java
call "%jar%" cf zbar.jar net\sourceforge\zbar\*.class

if "%OS%"=="Windows_NT" endlocal

pause