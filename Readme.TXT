fpcup
https://bitbucket.org/reiniero/fpcup/

What is it?
===========
fpcup is a tool that gets the current version of FreePascal Compiler (FPC) and Lazarus IDE from their subversion repositories and compiles/installs them.
It adds CHM documentation and configure the Lazarus help for you.

It also creates a shortcut on your desktop that points to the new Lazarus installation.

Meant to be used side by side with other FPC/Lazarus installations. It creates a separate primary config path directory for the new Lazarus installation, so it doesn't interfere with existing Lazarus installs.
It's open source software released under the LGPL with linking exception (same as FreePascal), and contains the Synapse libraries (BSD license). See source files for details.
All use permitted, also commercial, but no warranties, express or implied.
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

On OSX: only tested with XCode installed; no other requirements found.

How does it work?
=================
FPCUp is a fancy batch script written in FPC/Lazarus that simply downloads FPC and Lazarus from SVN and compiles them ;)
You run it, with options if necessary (see fpcup --help).
When done, it should have created a Lazarus_trunk shortcut on your desktop (Windows) or shell script in your home directory (Unix/Linux).
Start this to start your new Lazarus instance.
fpcup sets up a separate primary config path to store Lazarus settings separate from other installs.
You need to run the script/shortcut because it tells your Lazarus where to find that primary config path.

Hint: after the first run, you can add/remove packages, set up your settings as wanted etc.
You can then run fpcup --only=fpc,lazarus to only update FPC and Lazarus sources, which saves time, as you're not rebuilding help, installing packages, etc.
Of course, you could run the full version e.g. once per day to make sure all external repositories etc are up to date.

Technical explanation
=====================
If you want to improve or extend fpcup, here's a more detailed description of what it does.

Note that some steps will not be executed depending on the modules chosen by the user.
By default, all modules will be run; the help module is selected by default and pulls in bigide.
fpcup for Windows is 32 bit, but the crosswin32-64 module will be run so users can compile 64 bit applications with Lazarus.

1. Setup, checking/downloading prerequisites:
- fpcup creates a shortcut on your desktop (Windows) or home directory shell script to fpcup with all the options you chose, so you don't have to type it all again
- On Windows, it checks whether make.exe and unzip.exe are present in the binutils directory (default or user-specified).
If not, it downloads them.
- It checks if make.exe is GNU make, and not e.g. Delphi/Borland make.
- It checks whether a Subversion client is available. On Windows: if there isn't, it downloads one.
- If bootstrap compilers need to be downloaded, a certain unarchiver utility is needed, depending on the way the compiler is compressed.
fpcup checks unzip.exe for Windows, gnutar for OSX, and bunzip2 for Linux.
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
- When done, it creates a shortcut on your desktop (Windows) or shell script in home directory to the newly installed Lazarus
- It does the same using make bigide, in order to get as many .lpk files compiled and included as possible
- It creates a minimum configuration (environmentoptions.xml) in a separate primary-config-path, or updates that config with: the compiler, the FPC source directory, make location, gdb location, help files location.
- fpcup compiles the Lazarus documentation editor (LazDE) using lazbuild in the newly compiled Lazarus directory
- fpcup compiles the Lazarus data desktop using lazbuild in the newly compiled Lazarus directory

4. Getting help
- It compiles the Lazarus lhelp CHM viewer (LazDE) using lazbuild in the newly compiled Lazarus directory
- It gets the FPC chm documentation if not already present
- It compiles the Lazarus CHM help

5. Getting and compiling external modules
- It reads fpcup.ini and parses the items specified.
- It can download from HTTP and FTP sites, and use SVN to keep repositories up to date.
- It can register executables as external tools in Lazarus
- It can add packages to the IDE (needs Lazarus SVN r37443 (Lazarus 1.1 branch) or later)
- Please see fpcup.ini for details


Cross compiler extensions
=========================
fpcup has a facitility to extend its functionality using cross compiling modules. These are classes that inherit from the fpcup TCrossInstaller class defined in m_crossinstaller.pas
An example is the Windows 32=>Windows 64 cross compile unit in m_crosswin64.pas. Note that this is a simple version, as there is no need for separate binutils and library paths.
These modules get registered automatically when compiled into the fpcup code.
See fpcup.html for more details on implementing your own cross compilers and extensions.

LCL/FPC/Lazarus limitations
===========================
- Multiple widgetsets
The Lazarus LCL can be installed in different directories depending on architecture. This is very helpful when building cross compilers, as the compiled units and binutils do not interfere with each other.
fpcup uses this approach for building cross compilers.

However, there is no provision for separating LCL units for multiple widgetsets. If you compile e.g. GTK2 and QT LCL widgetsets for the same platform, one will overwite the other. This means that fpcup cannot manage multiple widgetsets either.
If you want to use multiple widgetsets, best practice is to use separated Lazarus directories (option --lazdir in fpcup).

- LCL documentation generation x64 Linux
Fpdoc from FPC 2.6 has known bugs; LCL .chm documentation generation on x64 Linux takes a long time; fpcup takes a huge amount of CPU, but eventually finished.

- LCL documentation generation x86 Windows
Fpdoc from FPC 2.6 has known bugs; LCL .chm documentation generation may take a long time or fail.

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

Additionally, on both Windows and Unix we add our own binutils to the beginning of the path when calling make.
This ensures that e.g. existing Delphi or cygwin make.exe does not mess with our installation.
This is probably a bigger problem on Windows than on Linux/Unix

Status
======
In development, but does work for me and others with e.g. FPC fixes_2.6 and Lazarus on Windows and Linux.


Contact
=======
For reporting bugs, suggestions, patches.
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

Version: based on commit 89f29320fee5
Build date: Thu Jun 28 15:29:19 2012 +0200
Compiled for CPU: i386 on Win32

Info: 28-6-2012 16:14:32: fpcup started.
DON'T PANIC!
Everything below is optional...

fpcup --<options>

fpcup can work with modules - see "only", "skip" below
List of all modules:
default
defaultwin32
crosswin32-64
defaultclean
defaultwin32clean
crosswin32-64Clean
defaultuninstall
defaultwin32uninstall
FPC
FPCuninstall
FPCclean
FPCCleanOnly
FPCGetOnly
FPCBuildOnly
lazarus
BIGIDE
lazbuild
USERIDE
lazarusuninstall
lazarusclean
BIGIDEclean
LazarusCleanOnly
LazarusGetOnly
LazarusBuildOnly
LazarusConfigOnly
helpfpc
helpfpcuninstall
helplazarus
helplazarusuninstall
HelpFPCCleanOnly
HelpLazarusCleanOnly
HelpFPCGetOnly
HelpLazarusGetOnly
HelpFPCBuildOnly
HelpFPCBuildOnly
HelpLazarusBuildOnly
HelpFPCConfigOnly
HelpLazarusConfigOnly
suggestedpackages
suggestedpackagesclean
suggestedpackagesuninstall
lhelp
lhelpclean
lhelpuninstall
lazdatadesktop
lazdatadesktopclean
lazdatadesktopuninstall
doceditor
doceditorclean
doceditoruninstall
synapse
synapseclean
synapseuninstall
lazarus_ccr
lazarus_ccrclean
lazarus_ccruninstall
fpspreadsheet
fpspreadsheetclean
fpspreadsheetuninstall
tiopf_source
tiopf_sourceclean
tiopf_sourceuninstall
tiopf_demos
tiopf_demosclean
tiopf_demosuninstall
tiopf_docs
tiopf_docsclean
tiopf_docsuninstall
tiopf
tiopfclean
tiopfuninstall
fpcdocs
fpcdocsclean
fpcdocsuninstall
lazres
lazresclean
lazresuninstall
synapsetrunk
synapsetrunkclean
synapsetrunkuninstall
pascalsane
pascalsaneclean
pascalsaneuninstall
leptonica
leptonicaclean
leptonicauninstall
tesseract
tesseractclean
tesseractuninstall
OCRivist
OCRivistclean
OCRivistuninstall
bigidepackages
bigidepackagesclean
bigidepackagesuninstall

The following modules run by default:
SUGGESTEDPACKAGES
LHELP
LAZDATADESKTOP
DOCEDITOR
FPCDOCS
LAZRES

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
                       Make sure it is not in the fpcdir directory
 clean                 Remove files created with build.
                       Can be combined with skip and only options.
 configfile=<filename> Load module definition file from <filename>.
                       Default: fpcup.ini in the program directory.
 cputarget=<name>      CPU target for cross_compiling.
                       <name> has to be one of the following:
                       i386,m68k,alpha,powerpc,powerpc64,
                       armeb,arm,sparc,x86_64,ia64
 fpcbootstrapdir=<dir> An existing FPC compiler is needed to compile the FPC
                       sources. Specify location with this option; if no
                       compiler found here, FPCUp will download one there.
                       Make sure it is not in the fpcdir directory
                       Default: c:\development\fpcbootstrap\
                       or ~\fpcbootstrap\
 fpcdir=<dir>          Target FPC dir, default c:\development\fpc\
                       or ~\fpc\
 fpcuplinkname=<name>  Name of the shortcut to the fpcup script.
                       On Windows: a desktop shortcut.
                       On other systems: a shell script in your home directory.
                       If empty specified, no shortcut will be produced.
                       Default: fpcup_update
                         or <lazlinkname>_update if lazlinkname specified
 fpcURL=<URL>          SVN URL from which to download; default: fixes_2.6:
                       http://svn.freepascal.org/svn/fpc/branches/fixes_2_6
 fpcOPT=<options>      Options passed on to the FPC make as OPT=options.
                       E.g.: --fpcOPT="-gl -dSAX_HTML_DEBUG -dUSE_MINGW_GDB"
 fpcrevision=<number>  Revert to FPC SVN revision <number>
 keeplocalchanges      Keep locally modified files (normally these would be
                       backed up as .diff files before doing svn revert.
 lazdir=<dir>          Target Lazarus dir, default c:\development\lazarus\
                       or ~\lazarus\
 lazlinkname=<name>    Name of the shortcut to the Lazarus install.
                       On Windows: a desktop shortcut.
                       On other systems: a shell script in your home directory.
                       If empty specified, no shortcut will be produced.
                       Default: Lazarus_trunk
 lazOPT=<options>      Options passed on to the Lazarus make as OPT=options.
 lazrevision=<number>  Revert to Lazarus SVN revision <number>
 lazURL=<URL>          SVN URL from which to download; default:
                       trunk (newest version):
                       http://svn.freepascal.org/svn/lazarus/trunk
 lclplatform=<name>    LCL widget set. <name> has to be one of the following:
                       carbon,fpgui,gtk,gtk2,qt,win32,wince
 noconfirm             No confirmation asked. For batch operation.
 only=<values>         Update/build or clean only the modules specified.
                       The module list is separated by commas.
                       See above for a list of modules.
 ostarget=<name>       OS target for cross_compiling.
                       <name> has to be one of the following:
                       darwin,freebsd,linux,netbsd,openbsd,os2,
                       solaris,wince,win32,win64
 primary-config-path=<dir>
                       Analogous to Lazarus primary-config-path parameter.
                       Determines where fpcup will create or use as primary
                       configuration path for the Lazarus it installs/updates.
                       Default: empty; then a OS dependent configuration
                       path is used; directory name lazarusdevsettings.
 skip=<values>         Do not update/build or clean modules.
                       The module list is separated by commas.
                       See above for a list of modules.
 uninstall             uninstall sources and all generated files
                       If no skip and only options given:
                       DELETE entire Lazarus/FPC directories
                       Else: uninstall only certain modules.
 verbose               Show output from svn and make.
 version               Show version info and quit.

Share and enjoy!

Info: 28-6-2012 16:14:32: fpcup finished.
