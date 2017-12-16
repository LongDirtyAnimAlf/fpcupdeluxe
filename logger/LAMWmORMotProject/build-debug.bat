set Path=%PATH%;C:\Users\Public\Documents\Embarcadero\Studio\17.0\PlatformSDKs\apache-ant-1.10.1\bin
set JAVA_HOME=C:\Program Files\Java\jdk1.8.0_25
call ant clean -Dtouchtest.enabled=true debug
if errorlevel 1 pause
