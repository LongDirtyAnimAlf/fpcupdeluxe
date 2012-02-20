fpcup
https://bitbucket.org/reiniero/fpcup/

What is it?
===========
fpcup is a tool that gets the current version of FreePascal Compiler (FPC) and Lazarus IDE from their subversion repositories and compiles/installs them.
It adds CHM documentation and configure the Lazarus help for you.

It also creates a shortcut on your desktop that points to the new Lazarus installation.

Meant to be used side by side with other FPC/Lazarus installations. It creates a separate primary config path directory for the new Lazarus installation, so it doesn't interfere with existing Lazarus installs.
It's open source software, please see the code for the license.
Run fpcup --help for command line options.

Why yet another tool?
=====================
I was disappointed in LazUpdater, a GUI program that has similar goals as the code seems to be too complex.
I also was tired of writing batch scripts and then having to press all kinds of buttons/fiddling with things to get it working.

Prerequisites
=============
On Windows: should be none.
If needed, this tool will download an svn client, the required binutils (make.exe etc) and a bootstrap FPC compiler for you.

On Linux:
- the binutils (make etc); e.g. in a package called build-essential
- bunzip2 (probably present in most distributions)
- unzip
- subversion client
E.g. on Debian or Ubuntu, do something like:
sudo aptitude install build-essential subversion unzip

On OSX: only tested with XCode installed; no requirements found

How does it work?
=================
FPCUp is a fancy batch script written in FPC/Lazarus ;)
You run it, with options if necessary (see fpcup --help).
When done, it should have created a Lazarus_trunk shortcut on your desktop (Windows) or shell script in your home directory (Unix/Linux).
Start this to start your new Lazarus instance.
FPCUP sets up a separate primary config path to store Lazarus settings separate from other installs.
You need to run the script/shortcut because it tells your Lazarus where to find that primary config path.

Detailed workflow
=================
1. Checking/downloading prerequisites:
- On Windows, it checks whether make.exe and unzip.exe are present in the binutils directory (default or user-specified).
If not, it downloads them.
- It checks if make.exe is GNU make, and not e.g. Delphi/Borland make.
- It checks whether a Subversion client is available. On Windows: if there isn't, it downloads one.
- If bootstrap compilers need to be downloaded, a certain unarchiver utility is needed, depending on the way the compiler is compressed.
FPCUp checks unzip.exe for Windows, gnutar for OSX, and bunzip2 for Linux.
- It checks if there is a valid FPC compiler in the bootstrap directory the user specified (or default).
If it doesn't exist, it downloads a bootstrap compiler.

2. Getting and compiling FPC
- It checks out or updates FPC using Subversion.
- It compiles FPC using the bootstrap compiler.
- It creates a valid fpc.cfg for the new compiler.
- On OSX/Linux: it creates a dummy fpc.sh designed to filter out existing system wide fpc.cfg (e.g. /etc/fpc.cfg) and places that in the FPC directory

3. Getting and compiling Lazarus
- It checks out or updates Lazarus using Subversion.
- It compiles Lazarus using the installed FPC compiler (Linux/OSX: fpc.sh) and fpc.cfg
- It does the same using make bigide, in order to get as many .lpk files compiled and included as possible
- It creates a minimum configuration (environmentoptions.xml) in a separate primary-config-path, or updates that config with: the compiler, the FPC source directory, make location, gdb location.
- When done, it creates a shortcut on your desktop (Windows) or shell script in home directory to the newly installed Lazarus

4. Compiling additional utilities & finishing up
- Fpcup compiles the Lazarus lhelp CHM viewer (LazDE) using lazbuild in the newly compiled Lazarus directory
- Fpcup compiles the Lazarus documentation editor (LazDE) using lazbuild in the newly compiled Lazarus directory
- Fpcup compiles the Lazarus data desktop using lazbuild in the newly compiled Lazarus directory
- When done, it will create a shortcut on your desktop (Windows) or home directory shell script to fpcup with all the options you chose, so you don't have to type it all again

Troubleshooting
===============
A big problem in designing this utility was to keep systemwide fpc.cfg on Unix/Linux from interfering with our FPC install.
It seems the only way around this is to write a fpc proxy (fpc.sh) that explicitly ignores any fpc.cfgs).

To check whether this works:
cd ~/<yourlazarusdirectory>
#Check system wide settings (look for FPC, FPC version, FPCDIR and UnitsDir:
make fpc_baseinfo | less
#Check (look for FPC, FPC version, FPCDIR and UnitsDir:
#this should pick up the proper unit path, fpcdir and version from the fpcup-installed fpc version
make fpc_baseinfo FPC=~/<yourfpcdirectory>/bin/fpc.sh
~/<yourfpcdirectory>/bin/fpc.sh -vut # or something?

Status
======
Still in development, but does work to keep my development copies of FPC fixes_2.6 and Lazarus up to date on Windows.


Contact
=======
For reportiing bugs, suggestions, patches.
- Lazarus forum thread:
http://lazarus.freepascal.org/index.php/topic,15701fpcup
- Bitbucket issue tracker
https://bitbucket.org/reiniero/fpcup/issues

Help output:
============
A recent fpcup --help shows this:

fpcup
An FPC/Lazarus downloader/updater/installer
Open source freeware (modified LGPL/BSD), see:
https://bitbucket.org/reiniero/fpcup

This program will download the FPC and Lazarus sources
from the source Subversion/SVN repositories,
compile, and install.
Result: you get a fresh, up-to-date Lazarus/FPC installation.
fpcup compiled with: FPC 2.6.1 on 2012/02/18 19:10:16
for CPU: i386 on Win32

fpcup --<options>
Options are not required; they include:
 help                  Show this text
 binutilsdir=<dir>     Windows only:
                       Directory where make, patch etc
                       (the binutils) are located. If make does not
                       exist, binutils will be downloaded there.
                       Default c:\development\fpcbootstrap\
                       Note: the binutils are copied to the
                       FPC directory for use by FPC. This gives
                       a more standard FPC environment.
 clean                 Don't build; clear out all modules
                       or limit cleaning to certain modules
                       (see "only" and "skip")
 fpcbootstrapdir=<dir> An existing FPC compiler is needed to compile the FPC
                       sources. Specify location with this option; if no
                       compiler found here, FPCUp will download one there.
                       Default: c:\development\fpcbootstrap\
 fpcdir=<dir>          Target FPC dir, default c:\development\fpc\
 fpcURL=<URL>          SVN URL from which to download; default: fixes_2.6:
                       http://svn.freepascal.org/svn/fpc/branches/fixes_2_6
 fpcOPT=<options>      Options passed on to the fpc make as OPT=options.
 fpcrevision=<number>  Revert to fpc svn revision <number>
 fpcuplinkname=<name>  Name of the shortcut to the fpcup script.
                       On Windows: a desktop shortcut.
                       On other systems: a shell script in your home directory.
                       If empty specified, no shortcut will be produced.
                       Default: fpcup_update
                         or <lazlinkname>_update if lazlinkname specified
 lazdir=<dir>          Target Lazarus dir, default c:\development\lazarus\
 lazlinkname=<name>    Name of the shortcut to the Lazarus install.
                       On Windows: a desktop shortcut.
                       On other systems: a shell script in your home directory.
                       If empty specified, no shortcut will be produced.
                       Default: Lazarus_trunk
 lazrevision=<number>  Revert to lazarus svn revision <number>
 lazURL=<URL>          SVN URL from which to download; default: 
                       trunk (newest version):
                       http://svn.freepascal.org/svn/lazarus/trunk
 lazOPT=<options>      Options passed on to the lazarus make as OPT=options.
 noconfirm             No confirmation asked. For batch operation. 
 only=<values>         update and build only the modules specified.
                       The module list is separated by commas and can contain:
                       FPC,lazarus,bigide,wincrossx64,doceditor,
                       lhelp,lazdatadesktop
 primary-config-path=<dir>
                       Analogous to Lazarus primary-config-path parameter.
                       Determines where fpcup will create or use as primary
                       configuration path for the Lazarus it installs/updates.
                       Default: empty; then a OS dependent configuration
                       directory is used.
 skip=<values>         Do not update or build modules.
                       The module list is separated by commas.
                       See "only" for accepted values. 
 verbose               Show output from svn and make

