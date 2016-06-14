unit m_any_to_linuxmipsel;
{ Cross compiles from Windows 32 to mipsel 32 bit (Little Endian)
Copyright (C) 2013 Reinier Olislagers

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at your
option) any later version with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a
module which is not derived from or based on this library. If you modify
this library, you may extend this exception to your version of the library,
but you are not obligated to do so. If you do not wish to do so, delete this
exception statement from your version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
for more details.

You should have received a copy of the GNU Library General Public License
along with this library; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{
Support for these binutils
1. Android
based on Android NDK=> apparently generates mipsel hardfloat
Add a cross directory under the fpcup "root" installdir directory (e.g. c:\development\cross, and e.g. regular fpc sources in c:\development\fpc)
Then place the directory layout provided by the crossfpc project there, so you get
...
c:\development\cross\bin\mipsel-linux
=> copy binutils here from a location like
android-ndk-r9c\toolchains\mipsel-linux-android-4.8\prebuilt\windows-x86_64\bin\mipsel-linux-android-as.exe
...
c:\development\cross\lib\mipsel-linux
=> copy your libs here

2. FPC-distributed GNU crossbinutils
based on cross binaries from
http://svn.freepascal.org/svn/fpcbuild/binaries/i386-win32/

Add a cross directory under the fpcup "root" installdir directory (e.g. c:\development\cross, and e.g. regular fpc sources in c:\development\fpc)
Then place the binaries in c:\development\cross\bin\mipsel-linux
Binaries include
mipsel-linux-ar.exe
mipsel-linux-as.exe
mipsel-linux-ld.exe
mipsel-linux-nm.exe
mipsel-linux-objcopy.exe
mipsel-linux-objdump.exe
mipsel-linux-strip.exe

3. CodeSourcery binutils

codesourcery libs: e.g. copy codesourcery mipsel softfloat uclibc libs in
c:\development\cross\bin\mipsel-linux
crtbegin.o
crtbeginS.o
crtbeginT.o
crtend.o
crtendS.o
crtfastmath.o
libgcc.a
libgcc_eh.a
libgcov.a
-->please replace/use the ones that work for your target machine
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller,fpcuputil,fileutil;

implementation
type

{ Tany_linuxmipsel }
Tany_linuxmipsel = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
  function TargetSignature: string;
public
  function GetLibs(Basepath:string):boolean;override;
  {$ifndef FPCONLY}
  function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;override;
  {$endif}
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ Twin32_linuxmipsel }
function Tany_linuxmipsel.TargetSignature: string;
begin
  result:=FTargetCPU+'-'+TargetOS;
end;

function Tany_linuxmipsel.GetLibs(Basepath:string): boolean;
const
  DirName='mipsel-linux';
  LibName='libc.so';
begin

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LibName);

  // first search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName);

  if result then
  begin
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Xd'+LineEnding+ {buildfaq 3.4.1 do not pass parent /lib etc dir to linker}
    '-Fl'+IncludeTrailingPathDelimiter(FLibsPath)+LineEnding+ {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    '-Xr/usr/lib';//+LineEnding+ {buildfaq 3.3.1: makes the linker create the binary so that it searches in the specified directory on the target system for libraries}
    //'-FL/usr/lib/ld-linux.so.2' {buildfaq 3.3.1: the name of the dynamic linker on the target};
    infoln('Twin32_linuxmipsel: found libspath '+FLibsPath,etInfo);
  end;
end;

{$ifndef FPCONLY}
function Tany_linuxmipsel.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least
  result:=true;
end;
{$endif}

function Tany_linuxmipsel.GetBinUtils(Basepath:string): boolean;
// You can copy the files from Android NDK, e.g.
// android-ndk-r9c\toolchains\mipsel-linux-android-4.8\prebuilt\windows-x86_64\bin\mipsel-linux-android-as.exe

// Also has support for codesourcery binutils
const
  DirName='mipsel-linux';
var
  AsFile: string;
begin
  inherited;

  AsFile:=FBinUtilsPrefix+'as'+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  {$ifdef unix}
  // User may also have placed them into their regular search path:
  if not result then { try /usr/local/bin/<dirprefix>/ }
    result:=SearchBinUtil('/usr/local/bin/'+DirName,
      AsFile);

  if not result then { try /usr/local/bin/ }
    result:=SearchBinUtil('/usr/local/bin',
      AsFile);

  if not result then { try /usr/bin/ }
    result:=SearchBinUtil('/usr/bin',
      AsFile);

  if not result then { try /bin/ }
    result:=SearchBinUtil('/bin',
      AsFile);
  {$endif unix}

  // Now also allow for mips-linux-gnu- binutilsprefix (e.g. codesourcery)
  if not result then
  begin
    FBinutilsPrefix:='mips-linux-gnu-';
    AsFile:=FBinUtilsPrefix+'as'+GetExeExt;
    result:=SearchBinUtil(FBinUtilsPath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
  end;

  {$ifdef unix}
  // User may also have placed them into their regular search path:
  if not result then { try /usr/local/bin/<dirprefix>/ }
    result:=SearchBinUtil('/usr/local/bin/'+DirName,
      AsFile);

  if not result then { try /usr/local/bin/ }
    result:=SearchBinUtil('/usr/local/bin',
      AsFile);

  if not result then { try /usr/bin/ }
    result:=SearchBinUtil('/usr/bin',
      AsFile);

  if not result then { try /bin/ }
    result:=SearchBinUtil('/bin',
      AsFile);
  {$endif unix}


  // Now also allow for mipsel-linux- binutilsprefix (e.g. using standard GCC crossbinutils)
  if not result then
  begin
    FBinutilsPrefix:='mipsel-linux-';
    AsFile:=FBinUtilsPrefix+'as'+GetExeExt;
    result:=SearchBinUtil(FBinUtilsPath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
  end;

  {$ifdef unix}
  // User may also have placed them into their regular search path:
  if not result then { try /usr/local/bin/<dirprefix>/ }
    result:=SearchBinUtil('/usr/local/bin/'+DirName,
      AsFile);

  if not result then { try /usr/local/bin/ }
    result:=SearchBinUtil('/usr/local/bin',
      AsFile);

  if not result then { try /usr/bin/ }
    result:=SearchBinUtil('/usr/bin',
      AsFile);

  if not result then { try /bin/ }
    result:=SearchBinUtil('/bin',
      AsFile);
  {$endif unix}

  // Now also allow for empty binutilsprefix:
  if not result then
  begin
    FBinutilsPrefix:='';
    AsFile:=FBinUtilsPrefix+'as'+GetExeExt;
    result:=SearchBinUtil(FBinUtilsPath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
  end;

  {$ifdef unix}
  // User may also have placed them into their regular search path:
  if not result then { try /usr/local/bin/<dirprefix>/ }
    result:=SearchBinUtil('/usr/local/bin/'+DirName,
      AsFile);

  if not result then { try /usr/local/bin/ }
    result:=SearchBinUtil('/usr/local/bin',
      AsFile);

  if not result then { try /usr/bin/ }
    result:=SearchBinUtil('/usr/bin',
      AsFile);

  if not result then { try /bin/ }
    result:=SearchBinUtil('/bin',
      AsFile);
  {$endif unix}

  if result then
  begin
    infoln(FCrossModuleName + ': found binutils '+FBinUtilsPath,etInfo);
    // Architecture etc:
    if StringListStartsWith(FCrossOpts,'-Cp')=-1 then
      FCrossOpts.Add('-CpMIPS32R2'); //Probably supported by most devices today
    // Softfloat unless otherwise specified (probably equivalent to -msoft-float for gcc):
    if StringListStartsWith(FCrossOpts,'-Cf')=-1 then
      FCrossOpts.Add('-CfSOFT');

    // Configuration snippet for FPC
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath)+LineEnding+ {search this directory for compiler utilities}
    '-XP'+FBinUtilsPrefix; {Prepend the binutils names}
  end
  else
  begin
    infoln(FCrossModuleName + ': could not find bin path. Please fill '+IncludeTrailingPathDelimiter(BasePath)+'..\cross\bin\'+DirName+LineEnding+
    ' with cross binutils for Android/Linux MIPSEL, such as mipsel-linux-android-as.exe '+LineEnding+
    ' e.g. from the Android NDK.'+LineEnding+
    'See http://wiki.lazarus.freepascal.org/MIPS.'
    ,etError);
    FAlreadyWarned:=true;
  end;
end;

constructor Tany_linuxmipsel.Create;
begin
  inherited Create;
  FCrossModuleName:='any_linuxmipsel';
  // binutilsprefix can be modified later in GetBinUtils  
  FBinUtilsPrefix:='mipsel-linux-android-'; //Used in Android NDK
  FBinUtilsPath:='';
  { Use current trunk compiler to build, not stable bootstrap, e.g. in light of bug
   http://bugs.freepascal.org/view.php?id=25399
  }
  FCompilerUsed:=ctInstalled;
  FFPCCFGSnippet:='';
  FLibsPath:='';
  FTargetCPU:='mipsel';
  FTargetOS:='linux';
  FAlreadyWarned:=false;
  infoln('Twin32_linuxmipsel crosscompiler loading',etDebug);
end;

destructor Tany_linuxmipsel.Destroy;
begin
  inherited Destroy;
end;

var
  Any_linuxmipsel:Tany_linuxmipsel;

initialization
  Any_linuxmipsel:=Tany_linuxmipsel.Create;
  RegisterExtension(Any_linuxmipsel.TargetCPU+'-'+Any_linuxmipsel.TargetOS,Any_linuxmipsel);
finalization
  Any_linuxmipsel.Destroy;
end.

