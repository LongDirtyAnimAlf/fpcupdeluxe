mkdir .\fpc-wrappers
mkdir .\fpc-wrappers\lib
mkdir .\fpc-wrappers\bin
del .\fpc-wrappers\lib\wrapper.o

set FPC=C:\fpcupsystems\trunk\fpc\bin\i386-win32\fpc.exe
set SYSTEM=-Twin64 -Px86_64

call %FPC% %SYSTEM% -dAR_STUB -o.\fpc-wrappers\bin\ar.exe -FU.\fpc-wrappers\lib stub.lpr
call %FPC% %SYSTEM% -dAS_STUB -o.\fpc-wrappers\bin\as.exe -FU.\fpc-wrappers\lib stub.lpr
call %FPC% %SYSTEM% -dNM_STUB -o.\fpc-wrappers\bin\nm.exe -FU.\fpc-wrappers\lib stub.lpr
call %FPC% %SYSTEM% -dOBJCOPY_STUB -o.\fpc-wrappers\bin\objcopy.exe -FU.\fpc-wrappers\lib stub.lpr
call %FPC% %SYSTEM% -dOBJDUMP_STUB -o.\fpc-wrappers\bin\objdump.exe -FU.\fpc-wrappers\lib stub.lpr
call %FPC% %SYSTEM% -dSTRIP_STUB -o.\fpc-wrappers\bin\strip.exe -FU.\fpc-wrappers\lib stub.lpr

echo "Done !"
