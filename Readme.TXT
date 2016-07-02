fpcup / fpclazup
Original: https://bitbucket.org/reiniero/fpcup/
New (this) : https://github.com/LongDirtyAnimAlf/Reiniero-fpcup

Acknowledgement
===========
As of August 28, 2015, this new repo contains all previous commits by Reinier.
Many thanks towards user "Arvur" (see forum) for making this possible.
Follow the fpcup development on http://forum.lazarus.freepascal.org/index.php/topic,27211.0.html

What is it?
===========
fpcup and fpclazup are basically wrappers around svn/make on steroids.
It tries to use the FPC/Lazarus build process as much as possible.

Fpcup is a tool that gets the FreePascal Compiler (FPC) from its subversion repository
and compiles/installs it.

Fpclazup is a tool that gets both the current version of FreePascal Compiler (FPC) and 
Lazarus IDE from their subversion repositories and compiles/installs them.
It adds CHM documentation and configures the Lazarus help for you.

From now on, fpcup ONLY handles FPC.

From now on, fpclazup has the same functionality as the previous fpcup: FPC + Lazarus !

Shortcut on your desktop are created that point to the new (Lazarus) installation.

Meant to be used side by side with other FPC/Lazarus installations. It creates a
separate primary config path directory for the new Lazarus installation, so it 
doesn't interfere with existing Lazarus installs.

It's open source software released under the LGPL with linking exception 
(same as FreePascal), and contains the Synapse libraries (BSD license). 
See source files for details.
All use permitted, also commercial, but no warranties, express or implied.
Run fpcup --help for command line options.


Prerequisites
=============

- Windows
=========
- none, *but* an SVN installation in your path (e.g. TortoiseSVN) is recommended
You can build Lazarus without an svn.exe in your path, but the Lazarus tool 
svn2revisioninc won't work and you won't get an SVN revision number in your 
Help/About box.
If needed, the tool will download all needed binaries (bootstrap compiler, 
binutils, svn executable)

- Linux
=======
- GNU make
- the binutils (make etc); e.g. in a package called build-essential
- bunzip2 (probably present in most distributions)
- unzip
- subversion client: svn
- gdb is not needed for building FPC/Lazarus but needed for debugging 
  your Lazarus programs 
- libX11, libgdk_pixbuf-2.0, libpango-1.0, libgdk-x11-2.0
E.g. on Debian or Ubuntu, do something like:
sudo aptitude install make binutils build-essential gdb subversion zip unzip libx11-dev libgtk2.0-dev gtk2-engines-pixbuf libcairo2-dev libpango1.0-0

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


How does it work?
=================
fpc(laz)up is a fancy batch script written in FPC/Lazarus that simply downloads FPC 
(and Lazarus) from SVN and compiles them ;)

You run it, with options if necessary (see fpclazup --help).
When done, it should have 
- created a Lazarus_trunk shortcut on your desktop (Windows/Linux) 
- a shell script in your home directory (*nix) linking to the installed Lazarus.
Start this to start your new Lazarus instance.

fpcup sets up a separate primary config path to store Lazarus settings 
separate from other installs.
You need to run the script/shortcut because it tells your Lazarus 
where to find that primary config path.

Hint: after the first run, you can add/remove packages, set up your settings 
as wanted etc.
You can then run fpclazup --only=fpc,lazarus to only update FPC and Lazarus sources
which saves time, as you're not rebuilding help, installing packages, etc.
Of course, you could run the full version e.g. once per day to make sure 
all external repositories etc are up to date.


Storing your settings
=====================
After running, fpcup creates a batch/shell script file by default to run it 
again with the same parameters.
On Windows, it creates a shortcut on your desktop (e.g. fpclazup_update) with those parameters.

Additionally, you can save the settings in an ini file and call 
fpclazup --inifile=settings.ini
(see the settings.ini example file)
The parameters not specified are default settings and can be overruled when 
calling from the command line:

[general]
fpcurl=http://svn.freepascal.org/svn/fpc/trunk
keeplocalchanges=true

As you can see, parameters like --keeplocalchanges that have no extra arguments 
on the command line versions are represented by adding =true to the parameter
in the ini file.
Parameters like fpcurl that do need an extra argument simply have it added.

Compiling
=========
If you want to compile fpcup from source, please compile hgversion.pas first; 
this should give a hgversion executable, which is used by the fpcup project 
build action (Compilation/Execute Before command) to generate the current fpcup 
version info into revision.inc
If you don't do this, you'll probably get incorrect version information in 
fpcup or compilation problems ("Can't open include file "revision.inc"").

Technical explanation
=====================
If you want to improve or extend fpcup, here's a more detailed description of 
what it does.

Note that some steps will not be executed depending on the modules chosen by 
the user.
By default, all modules will be run; the help module is selected by default and 
pulls in the useride module.
In the 32 bit fpclazup for Windows version, the crosswin32-64 module will be run so 
users can compile 64 bit applications with Lazarus.

For both fpcup and fpclazup:

1. Setup, checking/downloading prerequisites:
fpcup creates
- a shortcut on your desktop (Windows, Linux) 
- a home directory shell script to fpcup 
with all the options you chose, so you don't have to type it all again

- On Windows, it checks whether make.exe and unzip.exe are present in the 
binutils directory (default or user-specified). If not, it downloads them.

- It checks if make.exe is GNU make, and not e.g. Delphi/Borland make.

- It checks whether a Subversion client is available. 
On Windows: if there isn't, it downloads one.

- If bootstrap compilers need to be downloaded, an unarchiver utility is needed.
fpcup checks for existence of unzip, gnutar, or bunzip2 
depending on the format the compiler archive uses.

- It checks if there is a valid FPC compiler in the bootstrap directory 
the user specified (or default). 
If it doesn't exist, it downloads a bootstrap compiler.

2. Getting and compiling FPC
- It checks out or updates FPC using Subversion.

- It compiles FPC using the bootstrap compiler.

- It creates a valid fpc.cfg for the new compiler. It adds the compiler bin 
path and the one above it so tools like the fpcres resource compiler can be 
found.

- On FreeBSD/Linux/OSX: it creates a dummy fpc.sh designed to filter out 
existing system wide fpc.cfg (e.g. /etc/fpc.cfg) and places that 
in the FPC directory

For fpclazup only:

3. Getting and compiling Lazarus
- It checks out or updates Lazarus using Subversion.

- It compiles lazbuild (the command line Lazarus builder) using the installed 
FPC compiler (Unix/FreeBSD/Linux/OSX: fpc.sh) and fpc.cfg

- When done, it creates a shortcut on your desktop (Windows, Linux), and
a shell script in the home directory (*nix) to the newly installed Lazarus

- It compiles Lazarus, adding packages from fpcup.ini using make useride 
(or its lazbuild equivalent) in order to get a workable Lazarus with e.g. the 
database .lpk files compiled and included.

- It creates a minimum configuration (environmentoptions.xml) in a separate 
primary-config-path directory, or updates that config with: 
-- the compiler
-- the FPC source directory
-- make location
-- gdb/debugger location

- fpcup compiles the Lazarus documentation editor (LazDE) using 
lazbuild in the newly compiled Lazarus directory

- fpcup compiles the Lazarus data desktop using 
lazbuild in the newly compiled Lazarus directory

4. Getting help
- It compiles the Lazarus lhelp CHM viewer using 
lazbuild in the newly compiled Lazarus directory

- It gets the FPC chm documentation if not already present

- It compiles the Lazarus CHM help if it hasn't been compiled recently and if it
hasn't been marked read-only (reason: compiling this takes a long time)

5. Getting and compiling external modules
- It reads fpcup.ini and parses the items specified.

- It can download from HTTP and FTP sites, and use 
SVN, mercurial or git (if installed) to keep repositories up to date.

- It can register executables as external tools in Lazarus

- It can add packages to the IDE 
(needs Lazarus SVN r37443 (Lazarus 1.1 branch) or later)

- Please see fpcup.ini for details on external modules
(e.g. keywords that can be used)


Cross compiler extensions
=========================
fpc(laz)up has a facility to extend its functionality using cross compiling modules.
These are classes that inherit from the fpcup TCrossInstaller class defined 
in m_crossinstaller.pas

An example is the Windows 32=>Windows 64 cross compile unit in m_crosswin64.pas.
Note that this is a simple version, as there is no need for separate binutils and library paths.
A similar module is the *nix=>Windows 32/64 modules in 
m_anyinternallinker_to_win386.pas and m_anyinternallinker_to_win64.pas
These modules get registered automatically (using the unit's initialization 
section) when compiled into the fpcup code. 
You do need to add the unit to the fpcup.lpr project uses clause though.

To use them, you would specify 
--cputarget= (e.g. i386, x86_64... the way FPC defines the CPU names)
See fpcup -h for possible values
--ostarget= (e.g. freebsd, win32,...... the way FPC defines the OS names)

What you want to build e.g.
--only=FPCCleanOnly,FPCBuildOnly
(or leave this parameter for building FPC and the Lazarus LCL)

If there's a cross compiler available for the combination source+target OS/CPU, 
it will run. Otherwise you will get an error message.

For any platform that has an internal linker in FPC (FreeBSD, Linux,..) 
you can run a Windows cross compiler: 32 bit:
--cputarget=i386 --ostarget=win32 --only=FPCBuildOnly,LazarusBuildOnly
or Windows 64 bit:
--cputarget=x86_64 --ostarget=win32 --only=FPCBuildOnly,LazarusBuildOnly
When that is done, you can cross compile to Windows using Lazarus.

See fpcup.html for more details on implementing your own cross compilers and 
other extensions to fpcup.

LCL/FPC/Lazarus limitations
===========================
- Multiple widgetsets
The Lazarus LCL can be installed in different directories depending on 
architecture. This is very helpful when building cross compilers, as the 
compiled units and binutils do not interfere with each other.
fpcup uses this approach for building cross compilers.

However, there is no provision for separating LCL units for multiple widgetsets.
If you compile e.g. GTK2 and QT LCL widgetsets for the same platform, one will 
overwite the other. This means that fpcup cannot manage multiple widgetsets.
If you want to use multiple widgetsets, best practice is to use separated 
Lazarus directories (option --lazdir in fpcup).

- LCL documentation generation 
LCL .chm documentation generation on x64 Linux and x86 Windows may take a 
long time; fpcup takes a huge amount of CPU, but eventually finished.

Due to bugs in either the help sources or fpdoc, document generation may fail 
and generate 0 byte files, especially on FPC2.6.x.
Fpcup checks for this and will not overwrite existing files with empty files.

Troubleshooting
===============
A big problem in designing this utility was to keep systemwide fpc.cfg on 
Unix/Linux from interfering with our FPC install.
It seems the only way around this is to write an FPC proxy (fpc.sh) that 
explicitly ignores any fpc.cfgs.

To check whether this works:
cd ~/<yourlazarusdirectory>
#Check system wide settings (look for FPC, FPC version, FPCDIR and UnitsDir:
make fpc_baseinfo | less
#Check (look for FPC, FPC version, FPCDIR and UnitsDir:
#this should pick up the proper unit path, fpcdir and version from the 
#fpcup-installed fpc version
make fpc_baseinfo FPC=~/<yourfpcdirectory>/bin/fpc.sh
~/<yourfpcdirectory>/bin/fpc.sh -vut # or something?

Additionally, on both Windows and Unix we add our own binutils directory to the 
beginning of the path when calling make.
This ensures that e.g. existing Delphi or cygwin make.exe does not mess with 
our installation.
This is probably a bigger problem on Windows than on Linux/Unix.

Status
======
Works for me and others with e.g. FPC fixes_2.6 and Lazarus on Windows and Linux.
Testing versions for FreeBSD 9+ x64 and OSX available.


Contact
=======
For reporting bugs, suggestions, patches.
https://github.com/LongDirtyAnimAlf/Reiniero-fpcup/issues