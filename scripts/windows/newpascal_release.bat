del ..\NPLazBaseWin32.rar
del ..\CrossAndroidARM.rar
del ..\CrossLinuxAarch64.rar
del ..\CrossLinuxARM.rar
del ..\CrossLinuxi386.rar
del ..\CrossLinuxx64.rar
del ..\CrossWinceARM.rar

"C:\Program Files (x86)\WinRAR\Rar.exe" a -r -x..\fpc\.svn -x..\lazarus\.svn -x..\fpc\units -x..\fpc\bin -x..\cross -x..\newpascalbatchfiles -x..\apps -x..\ccr ..\NPLazBaseWin32.rar ..\
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\NPLazBaseWin32.rar ..\fpc\bin\i386-win32
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\NPLazBaseWin32.rar ..\fpc\bin\x86_64-win64
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\NPLazBaseWin32.rar ..\fpc\units\i386-win32
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\NPLazBaseWin32.rar ..\fpc\units\x86_64-win64
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\NPLazBaseWin32.rar ..\ccr\mORMot
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\NPLazBaseWin32.rar ..\mingw

"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossAndroidARM.rar ..\fpc\bin\arm-android
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossAndroidARM.rar ..\fpc\units\arm-android
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossAndroidARM.rar ..\cross\bin\arm-android
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossAndroidARM.rar ..\cross\lib\arm-android

"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossLinuxAarch64.rar ..\fpc\bin\aarch64-linux
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossLinuxAarch64.rar ..\fpc\units\aarch64-linux
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossLinuxAarch64.rar ..\cross\bin\aarch64-linux
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossLinuxAarch64.rar ..\cross\lib\aarch64-linux

"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossLinuxARM.rar ..\fpc\bin\arm-linux
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossLinuxARM.rar ..\fpc\units\arm-linux
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossLinuxARM.rar ..\cross\bin\arm-linux
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossLinuxARM.rar ..\cross\lib\arm-linux

"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossLinuxi386.rar ..\fpc\bin\i386-linux
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossLinuxi386.rar ..\fpc\units\i386-linux
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossLinuxi386.rar ..\cross\bin\i386-linux
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossLinuxi386.rar ..\cross\lib\i386-linux

"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossLinuxx64.rar ..\fpc\bin\x86_64-linux
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossLinuxx64.rar ..\fpc\units\x86_64-linux
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossLinuxx64.rar ..\cross\bin\x86_64-linux
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossLinuxx64.rar ..\cross\lib\x86_64-linux

"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossWinceARM.rar ..\fpc\bin\arm-wince
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossWinceARM.rar ..\fpc\units\arm-wince
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossWinceARM.rar ..\cross\bin\arm-wince
"C:\Program Files (x86)\WinRAR\Rar.exe" a -r ..\CrossWinceARM.rar ..\cross\lib\arm-wince

pause