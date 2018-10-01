@ECHO OFF
REM ###############################################
REM             fpcupdeluxe for windows
REM             darwin build app script
REM ###############################################

SET Zipper="%programfiles%\7-Zip\7z.exe"
SET BaseDir=.\..\..\deluxebin
SET BaseName=fpcupdeluxe-
SET FileList=%BaseName%i386-darwin-carbon %BaseName%i386-darwin-cocoa %BaseName%powerpc64-darwin-cocoa %BaseName%x86_64-darwin-cocoa %BaseName%x86_64-darwin-qt5

echo %Zipper%

FOR %%G IN (%FileList%) DO ( 
    if EXIST %BaseDir%\%%G.app\Contents\MacOS\%%G (
      del %BaseDir%\%%G.app\Contents\MacOS\%%G
      echo Deleted %BaseDir%\%%G.app\Contents\MacOS\%%G
    )
    if EXIST %BaseDir%\%%G.zip (
      del %BaseDir%\%%G.zip
      echo Deleted %BaseDir%\%%G.zip
    )
    if EXIST %BaseDir%\%%G (
      move /-y %BaseDir%\%%G %BaseDir%\%%G.app\Contents\MacOS
      echo Moved %BaseDir%\%%G into %BaseDir%\%%G.app\Contents\MacOS
      echo Zipping %BaseDir%\%%G.app ... please wait
      %Zipper% a -y -mx=5 %BaseDir%\%%G.zip %BaseDir%\%%G.app > nul
      echo Zipped %BaseDir%\%%G.app into %BaseDir%\%%G.zip 
    )
)

ECHO.
ECHO ===============================================
ECHO   darwin build app script ready
ECHO ===============================================
ECHO.
REM PAUSE