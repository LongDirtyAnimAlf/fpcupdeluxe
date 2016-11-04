@ECHO OFF
REM #####################################################
REM               fpclazup for windows
REM #####################################################

ECHO.
ECHO ====================================================
ECHO   Fpclazup for modules only:
ECHO   install a single module (zeos) only
ECHO   or install multiple modules only
ECHO ====================================================
ECHO.

if EXIST .\fpclazup.exe (
fpclazup.exe --only="zeos" --verbose
)

REM fpclazup.exe --only="lazpaint,bgracontrols,bgragames,ecc,indy,turbobird,notepas,uos,lazradio,treelistview" --verbose
ECHO.
ECHO ====================================================
ECHO   Fpclazup for modules only
ECHO ====================================================
ECHO.
PAUSE