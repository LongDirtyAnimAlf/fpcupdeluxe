@if "%DEBUG%" == "" @echo off
@rem ##########################################################################
@rem
@rem  Android libiconv buildscript for Windows
@rem  Please set the path towards the NDK
@rem
@rem ##########################################################################

@rem Set local scope for the variables with windows NT shell
if "%OS%"=="Windows_NT" setlocal


@rem set ndk=C:\Users\Public\Documents\Embarcadero\Studio\17.0\PlatformSDKs\android-ndk-r10e
@rem set ndk=C:\Users\Alfred\AppData\Local\Android\android-ndk-r19c
@rem set ndk=C:\Users\Alfred\AppData\Local\Android\android-ndk-r20b
set ndk=C:\Users\Alfred\AppData\Local\Android\Sdk\ndk\21.1.6352462

set path=%ndk%;%path%

@rem echo path

call %ndk%\ndk-build.cmd clean
call %ndk%\ndk-build.cmd all

if "%OS%"=="Windows_NT" endlocal

pause