fpcup / fpclazup / fpcupdeluxe

Original: https://bitbucket.org/reiniero/fpcup/
Updated : https://github.com/LongDirtyAnimAlf/Reiniero-fpcup
New (this) : https://github.com/newpascal/fpcupdeluxe
Wiki : http://wiki.freepascal.org/fpcupdeluxe

Acknowledgement
===========
As of August 28, 2015, this new repo contains all previous commits by Reinier.
Many thanks towards user "Arvur" (see forum) for making this possible.
Follow the fpcup development on http://forum.lazarus.freepascal.org/index.php/topic,27211.0.html

What is it?
===========
Fpcup, fpclazup and fpcupdeluxe are basically wrappers around svn/make on steroids.
They try to use the FPC/Lazarus build process as much as possible.

Fcpupdeluxe is the latest addition that adds a GUI to ease its use.

Shortcut on your desktop are created that point to the new (Lazarus) installation.

Meant to be used side by side with other FPC/Lazarus installations. It creates a
separate primary config path directory for the new Lazarus installation, so it 
doesn't interfere with existing Lazarus installs.

It's open source software released under the LGPL with linking exception 
(same as FreePascal), and contains some open source libraries with their own license. 
See source files for details.
All use permitted, also commercial, but no warranties, express or implied.

Prerequisites
=============

- Windows
=========
- none
If needed, the tool will download all needed binaries (bootstrap compiler, 
binutils, svn executable)

- Linux
=======
- GNU make
- the binutils (make etc); e.g. in a package called build-essential
- bunzip2 (probably present in most distributions)
- unzip
- untar
- subversion client: svn
- gdb is not needed for building FPC/Lazarus but needed for debugging 
  your Lazarus programs 
- libX11, libgdk_pixbuf-2.0, libpango-1.0, libgdk-x11-2.0
E.g. on Debian or Ubuntu, do something like:
sudo aptitude install make binutils build-essential gdb subversion zip unzip libx11-dev libgtk2.0-dev libgdk-pixbuf2.0-dev libcairo2-dev libpango1.0-dev

- Apple OSX
===========
- Xcode and Xcode command line tools

- FreeBSD 9+
=============
- none, but *strongly recommended* to use a newer gdb than the 6.1 version 
supplied with the system, e.g. by
cd /usr/ports/devel/gdb
make -DBATCH install clean
Use gdb in /usr/local/bin/gdb
- for Lazarus, you'll need XWindows with GTK (default) or Qt


Cross compiler extensions
=========================
Fpcupdeluxe has a facility to extend its functionality building and using cross compiling modules.

Contact
=======
For reporting bugs, suggestions, patches.
https://github.com/newpascal/fpcupdeluxe/issues
