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
  Classes, SysUtils;

implementation

uses
  FileUtil, m_crossinstaller, fpcuputil;

type

{ Tany_linuxmipsel }
Tany_linuxmipsel = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
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

function Tany_linuxmipsel.GetLibs(Basepath:string): boolean;
begin
  result:=inherited;
  if result then exit;

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LIBCFILENAME);

  {$IFDEF UNIX}
  // search local default cross-libs paths
  if not result then
    result:=SearchLibrary('/usr/mips-linux-gnu/lib',LIBCFILENAME);
  {$ENDIF}

  // first search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,LIBCFILENAME);

  SearchLibraryInfo(result);

  if result then
  begin
    FLibsFound:=True;
    //todo: check if -XR is realy needed for fpc root dir Prepend <x> to all linker search paths
    AddFPCCFGSnippet('-Xd'); {buildfaq 3.4.1 do not pass parent /lib etc dir to linker}
    AddFPCCFGSnippet('-Fl'+LibsPath); {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    //AddFPCCFGSnippet('-XR'+LibsPath); {buildfaq 1.6.4/3.3.1: the directory to look for the target libraries ... just te be safe ...}
    AddFPCCFGSnippet('-Xr/usr/lib');
  end;
end;

{$ifndef FPCONLY}
function Tany_linuxmipsel.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least
  result:=inherited;
end;
{$endif}

function Tany_linuxmipsel.GetBinUtils(Basepath:string): boolean;
// You can copy the files from Android NDK, e.g.
// android-ndk-r9c\toolchains\mipsel-linux-android-4.8\prebuilt\windows-x86_64\bin\mipsel-linux-android-as.exe
// Also has support for codesourcery binutils
var
  AsFile: string;
  BinPrefixTry:string;
begin
  result:=inherited;
  if result then exit;

  AsFile:=BinUtilsPrefix+ASFILENAME+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);

  {$IFDEF UNIX}
  // Now also allow for empty binutilsprefix in the right directory:
  if not result then
  begin
    BinPrefixTry:='';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;
    // search local default cross-utils paths
    result:=SearchBinUtil('/usr/mips-linux-gnu/bin',AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;
  {$ENDIF}

  if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  // Now also allow for mipsel-linux-gnu
  if not result then
  begin
    BinPrefixTry:='mipsel-linux-gnu-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  // Now also allow for mips-linux-gnu- binutilsprefix (e.g. codesourcery)
  if not result then
  begin
    BinPrefixTry:='mips-linux-gnu-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  // Now also allow for mips-linux-uclibc binutilsprefix (e.g. using standard GCC crossbinutils)
  if not result then
  begin
    BinPrefixTry:='mips-linux-uclibc-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  // Now also allow for mips-openwrt-linux-uclibc binutilsprefix
  if not result then
  begin
    BinPrefixTry:='mips-openwrt-linux-uclibc-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  // Now also allow for empty binutilsprefix:
  if not result then
  begin
    BinPrefixTry:='';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  SearchBinUtilsInfo(result);

  if result then
  begin
    FBinsFound:=true;
    // Configuration snippet for FPC
    AddFPCCFGSnippet('-FD'+BinUtilsPath);
    AddFPCCFGSnippet('-XP'+BinUtilsPrefix);
  end
  else
  begin
    FAlreadyWarned:=true;
  end;
end;

constructor Tany_linuxmipsel.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.mipsel;
  FTargetOS:=TOS.linux;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor Tany_linuxmipsel.Destroy;
begin
  inherited Destroy;
end;

var
  Any_linuxmipsel:Tany_linuxmipsel;

initialization
  Any_linuxmipsel:=Tany_linuxmipsel.Create;
  RegisterCrossCompiler(Any_linuxmipsel.RegisterName,Any_linuxmipsel);

finalization
  Any_linuxmipsel.Destroy;
end.

