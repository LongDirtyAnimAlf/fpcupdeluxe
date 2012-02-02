fpcup
https://bitbucket.org/reiniero/fpcup/

What is it?
===========
A tool that gets the current version of FreePascal Compiler (FPC) and Lazarus IDE from their subversion repositories and compiles/installs them.
It also creates a shortcut on your desktop that points to the new Lazarus installation.
Meant to be used side by side with other FPC/Lazarus installations (it creates a separate primary config path directory for the new Lazarus installation, so it doesn't interfere with existing Lazarus installs)
It's open source software, please see the code for the license.

Why yet another tool?
=====================
I was disappointed in LazUpdater, a GUI program that has similar goals as the code seems to be too complex.
I also was tired of writing batch scripts and then having to press all kinds of buttons/fiddling with things to get it working.

Prerequisites
=============
On Windows: should be none. If needed, this tool will download an svn client, the required binutils (make.exe etc) and a bootstrap FPC compiler for you.
On Linux/OSX: the binutils (make etc), and an svn client.
E.g. on Debian:
aptitude install build-essential subversion

Status
======
Still in development, but does work to keep my development copies of FPC fixes_2.6 and Lazarus up to date on Windows.

Busy adapting it to Linux/OSX and bugfixing Windows bootstrap functionality.
Patches/updates welcome, pleas see 
https://bitbucket.org/reiniero/fpcup/issues

Lazarus forum thread:
http://lazarus.freepascal.org/index.php/topic,15701