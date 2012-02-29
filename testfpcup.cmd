@echo off
rem Testfpcup with fixed lazarus, fpc SVN repositories
rem Placed in separate directory
rem You can pass additional arguments (--verbose etc) to fpcup if you want to.
fpcup --lazURL=http://svn.freepascal.org/svn/lazarus/tags/lazarus_0_9_30_2/ --fpcURL=http://svn.freepascal.org/svn/fpc/tags/release_2_6_0/ --fpcdir=c:\development\fpc_fpcuptest --lazdir=c:\development\lazarus_fpcuptest --primary-config-path=C:\development\lazarussettings_fpcuptest --lazlinkname=Lazarus_FPCUPTest --noconfirm %*