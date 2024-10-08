mkdir .\fpc-wrappers
mkdir .\fpc-wrappers\lib
del .\fpc-wrappers\lib\wrapper.o

set FPC=C:\fpcupsystems\trunk\fpc\bin\i386-win32\fpc.exe
set SYSTEM=-Twin64 -Px86_64

set TARGET=clang.exe

call %FPC% %SYSTEM% -damd64_darwin -dCLANG -o.\fpc-wrappers\x86_64-apple-darwin19-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -darm64_darwin -dCLANG -o.\fpc-wrappers\aarch64-apple-darwin19-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -di386_darwin -dCLANG -o.\fpc-wrappers\i386-apple-darwin19-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -darm64_ios -dCLANG -o.\fpc-wrappers\aarch64-apple-ios14-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -darm_ios -dCLANG -o.\fpc-wrappers\arm-apple-ios10-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr

set TARGET=clang++.exe

call %FPC% %SYSTEM% -damd64_darwin -dCLANGPP -o.\fpc-wrappers\x86_64-apple-darwin19-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -darm64_darwin -dCLANGPP -o.\fpc-wrappers\aarch64-apple-darwin19-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -di386_darwin -dCLANGPP -o.\fpc-wrappers\i386-apple-darwin19-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -darm64_ios -dCLANGPP -o.\fpc-wrappers\aarch64-apple-ios14-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -darm_ios -dCLANGPP -o.\fpc-wrappers\arm-apple-ios10-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr

set TARGET=ld.exe

call %FPC% %SYSTEM% -damd64_darwin -dLD -o.\fpc-wrappers\x86_64-apple-darwin19-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -darm64_darwin -dLD -o.\fpc-wrappers\aarch64-apple-darwin19-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -di386_darwin -dLD -o.\fpc-wrappers\i386-apple-darwin19-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -darm64_ios -dLD -o.\fpc-wrappers\aarch64-apple-ios14-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -darm_ios -dLD -o.\fpc-wrappers\arm-apple-ios10-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr

set TARGET=dsymutil.exe

call %FPC% %SYSTEM% -damd64_darwin -dDSYMUTIL -o.\fpc-wrappers\x86_64-apple-darwin19-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -darm64_darwin -dDSYMUTIL -o.\fpc-wrappers\aarch64-apple-darwin19-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -di386_darwin -dDSYMUTIL -o.\fpc-wrappers\i386-apple-darwin19-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -darm64_ios -dDSYMUTIL -o.\fpc-wrappers\aarch64-apple-ios14-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -darm_ios -dDSYMUTIL -o.\fpc-wrappers\arm-apple-ios10-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr

set TARGET=strip.exe

call %FPC% %SYSTEM% -damd64_darwin -dSTRIP -o.\fpc-wrappers\x86_64-apple-darwin19-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -darm64_darwin -dSTRIP -o.\fpc-wrappers\aarch64-apple-darwin19-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -di386_darwin -dSTRIP -o.\fpc-wrappers\i386-apple-darwin19-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -darm64_ios -dSTRIP -o.\fpc-wrappers\aarch64-apple-ios14-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr
call %FPC% %SYSTEM% -darm_ios -dSTRIP -o.\fpc-wrappers\arm-apple-ios10-%TARGET% -FU.\fpc-wrappers\lib wrapper.lpr

echo "Done !"
