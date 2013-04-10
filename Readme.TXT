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
Note: you can build Lazarus without svn being in your path, but the Lazarus tool svn2revisioninc won't work and you won't get an SVn revision number in your Help/About box.

On Linux:
- the binutils (make etc); e.g. in a package called build-essential
- bunzip2 (probably present in most distributions)
- unzip
- subversion client
E.g. on Debian or Ubuntu, do something like:
sudo aptitude install build-essential subversion unzip

On FreeBSD 9+:
- should be none
- for Lazarus, you'll need XWindows with GTK (default) or Qt

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


Storing your settings
=====================
After running, fpcup creates a batch/shell script file by default to run it again with the same parameters.

Additionally, you can save the settings in an ini file and call fpcup --inifile=settings.ini
(see the settings.ini examle file)
The parameters not specified are default settings and can be overruled when calling from the command line:

[general]
fpcurl=http://svn.freepascal.org/svn/fpc/trunk
keeplocalchanges=true

As you can see, parameters like --keeplocalchanges that have no arguments on the command line versions are represented like keeplocalchanges=true in the ini file.

Compiling
=========
If you want to compile fpcup from source, please compile hgversion.pas first; this should give a hgversion executable, which is used by the fpcup project build action (Compilation/Execute Before command) to generate the current fpcup version info into revision.inc
If you don't do this, you'll probably get incorrect version information in fpcup or compilation problems ("Can't open include file "revision.inc"").

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
fpcup checks unzip, gnutar, and bunzip2 depending on the file format.
- It checks if there is a valid FPC compiler in the bootstrap directory the user specified (or default).
If it doesn't exist, it downloads a bootstrap compiler.

2. Getting and compiling FPC
- It checks out or updates FPC using Subversion.
- It compiles FPC using the bootstrap compiler.
- It creates a valid fpc.cfg for the new compiler.
- On FreeBSD/Linux/OSX: it creates a dummy fpc.sh designed to filter out existing system wide fpc.cfg (e.g. /etc/fpc.cfg) and places that in the FPC directory

3. Getting and compiling Lazarus
- It checks out or updates Lazarus using Subversion.
- It compiles Lazarus using the installed FPC compiler (FreeBSD/Linux/OSX: fpc.sh) and fpc.cfg
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
fpcup has a facility to extend its functionality using cross compiling modules. These are classes that inherit from the fpcup TCrossInstaller class defined in m_crossinstaller.pas
An example is the Windows 32=>Windows 64 cross compile unit in m_crosswin64.pas. Note that this is a simple version, as there is no need for separate binutils and library paths.
These modules get registered automatically when compiled into the fpcup code.

To use them, you would specify 
--cputarget= (e.g. i386, x86_64... the way FPC defines the CPU names)
--ostarget= (e.g. freebsd, win32,...... the way FPC defines the OS names)
What you want to build e.g.
--only=FPCBuildOnly,LazarusBuildOnly
If there's a cross compiler available for the combination source+target OS/CPU, it will run. Otherwise you will get an error message.

For any platform that as an internal linker in FPC (FreeBSD, Linux,..) you can run a Windows cross compiler:
--cputarget=i386 --ostarget=win32 --only=FPCBuildOnly,LazarusBuildOnly
When that is done, you can cross compile to Windows using Lazrus.

See fpcup.html for more details on implementing your own cross compilers and extensions.

LCL/FPC/Lazarus limitations
===========================
- Multiple widgetsets
The Lazarus LCL can be installed in different directories depending on architecture. This is very helpful when building cross compilers, as the compiled units and binutils do not interfere with each other.
fpcup uses this approach for building cross compilers.

However, there is no provision for separating LCL units for multiple widgetsets. If you compile e.g. GTK2 and QT LCL widgetsets for the same platform, one will overwite the other. This means that fpcup cannot manage multiple widgetsets either.
If you want to use multiple widgetsets, best practice is to use separated Lazarus directories (option --lazdir in fpcup).

- LCL documentation generation 
Fpdoc from FPC 2.6 has known bugs, some of which are fixed in FPC 2.7.
LCL .chm documentation generation on x64 Linux and x86 Windows may take a long time; fpcup takes a huge amount of CPU, but eventually finished.

Due to bugs in either the help sources or fpdoc, document generation may fail and generate 0 byte files, especially on FPC2.6.x. Fpcup checks for this and will not overwrite existing help files with empty files.

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

Additionally, on both Windows and Unix we add our own binutils directory to the beginning of the path when calling make.
This ensures that e.g. existing Delphi or cygwin make.exe does not mess with our installation.
This is probably a bigger problem on Windows than on Linux/Unix

Status
======
Works for me and others with e.g. FPC fixes_2.6 and Lazarus on Windows and Linux. Testing versions for FreeBSD 9+ x64 and OSX available.


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

Version: based on commit 639611f10ea9+ (Wed Apr 10 08:13:31 2013 +0200)
Build date: 2013/04/10 09:55:27
Compiled for CPU: i386 on win32

DON'T PANIC!
Everything below is optional...

fpcup --<options>

fpcup can work with modules - see "only", "skip" below
List of all modules:
bgracontrols
bgracontrolsclean
bgracontrolsuninstall
BIGIDE
BIGIDEclean
bigidepackages
bigidepackagesclean
bigidepackagesuninstall
brookframework
brookframeworkclean
brookframeworkuninstall
crosswin32-64
crosswin32-64Clean
crosswin64-32
crosswin64-32Clean
default
defaultclean
defaultuninstall
defaultwin32
defaultwin32clean
defaultwin32uninstall
defaultwin64
defaultwin64uninstall
doceditor
doceditorclean
doceditoruninstall
fblib
fblibclean
fblibuninstall
FPC
FPCBuildOnly
FPCclean
FPCCleanOnly
fpcdocs
fpcdocsclean
fpcdocsuninstall
FPCGetOnly
FPCuninstall
fpcup
fpcupclean
fpcupuninstall
fpspreadsheet
fpspreadsheetclean
fpspreadsheetuninstall
greyhound
greyhoundclean
greyhounduninstall
helpfpc
HelpFPCBuildOnly
HelpFPCCleanOnly
HelpFPCConfigOnly
HelpFPCGetOnly
helpfpcuninstall
helplazarus
HelpLazarusBuildOnly
helplazarusclean
HelpLazarusConfigOnly
HelpLazarusGetOnly
helplazarusuninstall
installerlazwin
installerlazwinclean
installerlazwinuninstall
kzdesktop
kzdesktopclean
kzdesktopuninstall
lazarus
lazarus_ccr
lazarus_ccrclean
lazarus_ccruninstall
LazarusBuildOnly
lazarusclean
LazarusCleanOnly
LazarusConfigOnly
LazarusGetOnly
lazarusuninstall
lazbuild
lazdatadesktop
lazdatadesktopclean
lazdatadesktopuninstall
lazpackager
lazpackagerclean
lazpackageruninstall
lazpaint
lazpaintclean
lazpaintuninstall
lazres
lazresclean
lazresuninstall
leptonica
leptonicaclean
leptonicauninstall
lhelp
lhelpclean
lhelpuninstall
ljgridutils
ljgridutilsclean
ljgridutilsuninstall
lnet
lnetclean
lnetuninstall
mupdf
mupdfclean
mupdfuninstall
OCRivist
OCRivistclean
OCRivistuninstall
pascalsane
pascalsaneclean
pascalsaneuninstall
rx
rxclean
rxuninstall
suggestedpackages
suggestedpackagesclean
suggestedpackagesuninstall
synapse
synapseclean
synapsetrunk
synapsetrunkclean
synapsetrunkuninstall
synapseuninstall
tesseract
tesseractclean
tesseractuninstall
tiopf
tiopfclean
tiopfuninstall
USERIDE
vampyre
vampyreclean
vampyreuninstall
zeostesting
zeostestingclean
zeostestinguninstall

The following modules run by default:
SUGGESTEDPACKAGES
LAZDATADESKTOP
DOCEDITOR
FPCDOCS
LAZRES
LAZPACKAGER

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
                       Accepts shortcuts: 2.7.1,trunk,2.6.3,fixes,2.6.2,2.6.0
 fpcOPT=<options>      Options passed on to the FPC make as OPT=options.
                       E.g.: --fpcOPT="-gl -dSAX_HTML_DEBUG -dUSE_MINGW_GDB"
 fpcrevision=<number>  Revert to FPC SVN revision <number>
 keeplocalchanges      Keep locally modified files (normally these would be
                       backed up as .diff files before doing svn revert.
 inifile=<file>        Reads in ini file with options.
                       Example ini file:
[General]
help=true ; Use true for options that do not normally have arguments
binutilsdir=c:\dev\binutils
                       Options can be overwritten by command line parameters.

 inifilesection=<sec>  Section name to be used if an @file is specified.
                       If not given, use [General]
 installdir=<dir>      Base installation dir. FPC will install in <dir>\fpc\,
                       Lazarus in <dir>\lazarus\, bootstrap compiler in 
                       <dir>\fpcbootstrap\, (Windows only) binutils in 
                       <dir>\fpcbootstrap\, extra modules in <dir>\extras\ 
                       and configuration in <dir>\config\. See fpcdir, lazdir,
                       fpcbootstrapdir, binutilsdir, primary-config-path
                       for the defaults when installdir is not specified.
                       You can also use these to override the defaults given
                       by installdir.
 lazdir=<dir>          Target Lazarus dir, default c:\development\lazarus\
                       or ~\lazarus\
 lazlinkname=<name>    Name of the shortcut to the Lazarus install.
                       On Windows: a desktop shortcut.
                       On other systems: a shell script in your home directory.
                       If empty specified, no shortcut will be produced.
                       Default: depends on Lazarus directory
 lazOPT=<options>      Options passed on to the Lazarus make as OPT=options.
 lazrevision=<number>  Revert to Lazarus SVN revision <number>
 lazURL=<URL>          SVN URL from which to download; default: 
                       trunk (newest version):
                       http://svn.freepascal.org/svn/lazarus/trunk
                       Accepts shortcuts: trunk,1.1,1.0.8,1.0.6,1.0
 lclplatform=<name>    LCL widget set. <name> has to be one of the following:
                       carbon,fpgui,gtk,gtk2,qt,win32,wince
 logfilename=<file>    Location of log file. If nothing specified,
                       fpcup.log in the current directory.
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

Info: 10-4-2013 9:56:18: fpcup finished.
