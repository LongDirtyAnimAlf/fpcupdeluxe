unit m_win32_to_wincearm;
{ Cross compiles from Windows 32 to Windows CE
(Windows Embedded/Windows CE/Windows mobile) on the ARM processor
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
Setup: based on cross binaries from
http://svn2.freepascal.org/svn/fpcbuild/binaries/i386-win32/
with binutils 2.22

- superseding those from
ftp://ftp.freepascal.org/pub/fpc/contrib/cross/binutils-2.15.94-win32-arm-wince.zip
(ftp version: make sure cygwin1.dll is in your path or put it in the c:\development\cross\bin\arm-wince directory)

Add a cross directory under the fpcup "root" installdir directory (e.g. c:\development\cross, and e.g. regular fpc sources in c:\development\fpc)
Then place the binaries in c:\development\cross\bin\arm-wince
Binaries include
arm-wince-ar.exe
arm-wince-as.exe
arm-wince-dlltool.exe
arm-wince-ld.exe
arm-wince-nm.exe
arm-wince-objcopy.exe
arm-wince-objdump.exe
arm-wince-strip.exe
arm-wince-windres.exe
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller,fpcuputil;

implementation
type

{ TWin32_wincearm }
TWin32_wincearm = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
  function TargetSignature: string;
public
  function GetLibs(Basepath:string):boolean;override;
  function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TWin32_wincearm }
function TWin32_wincearm.TargetSignature: string;
begin
  result:=FTargetCPU+'-'+TargetOS;
end;

function TWin32_wincearm.GetLibs(Basepath:string): boolean;
const
  DirName='arm-wince';
begin
  // Wince does not need libs by default, but user can add them.
  FLibsPath:=SafeExpandFileName(IncludeTrailingPathDelimiter(BasePath)+'lib\'+DirName);
  result:=DirectoryExists(IncludeTrailingPathDelimiter(BasePath)+FLibsPath);
  if not result then
  begin
    // Show path info etc so the user can fix his setup if errors occur
    infoln('TWin32_wincearm: failed: searched libspath '+FLibsPath,etInfo);
    FLibsPath:=SafeExpandFileName(IncludeTrailingPathDelimiter(BasePath)+'..\cross\lib\'+DirName);
    result:=DirectoryExists(FLibsPath);
    if not result then
      infoln('TWin32_wincearm: failed: searched libspath '+FLibsPath,etInfo);
  end;
  if result then
  begin
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Fl'+IncludeTrailingPathDelimiter(FLibsPath) {buildfaq 1.6.4/3.3.1:  the directory to look for the target  libraries};
    infoln('TWin32_wincearm: found libspath '+FLibsPath,etInfo);
  end;
  if not result then
  begin
    //libs path is optional; it can be empty
    infoln('TWin32_wincearm: libspath ignored; it is optional for this cross compiler.',etInfo);
    FLibsPath:='';
    result:=true;
  end;
end;

function TWin32_wincearm.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least, add to FFPCCFGSnippet
  infoln('todo: implement lcl libs path from basepath '+BasePath,etdebug);
  result:=true;
end;

function TWin32_wincearm.GetBinUtils(Basepath:string): boolean;
const
  DirName='arm-wince';
var
  AsFile: string;
begin
  inherited;
  AsFile:=FBinUtilsPrefix+'as.exe';
  result:=SearchBinUtil(IncludeTrailingPathDelimiter(FBinUtilsPath),
    AsFile);

  // Using default naming (svn2 repo)
  if not result then
    FBinUtilsPath:=IncludeTrailingPathDelimiter(BasePath)+'bin'+DirectorySeparator+DirName;
  if not result then
    result:=SearchBinUtil(IncludeTrailingPathDelimiter(FBinUtilsPath),
      AsFile);

  // cross\bin
  if not result then
    FBinUtilsPath:=SafeExpandFileName(IncludeTrailingPathDelimiter(BasePath)+'..\cross\bin\'+DirName);
  if not result then
    result:=SearchBinUtil(IncludeTrailingPathDelimiter(FBinUtilsPath),
      AsFile);

  // Search for FTP (old) version
  if not result then
  begin
    FBinUtilsPrefix:='arm-wince-pe-';
    AsFile:=FBinUtilsPrefix+'as.exe';
    FBinUtilsPath:=IncludeTrailingPathDelimiter(BasePath)+'bin'+DirectorySeparator+DirName;
  end;
  if not result then
    result:=SearchBinUtil(IncludeTrailingPathDelimiter(FBinUtilsPath),
      AsFile);

  //todo: remove all fbinutilspath assignments before searchbinutil as searchbinutil changes fbinutilspath anyway
  // cross\bin
  if not result then
    result:=SearchBinUtil(SafeExpandFileName(IncludeTrailingPathDelimiter(BasePath)+'..\cross\bin\'+DirName),
      AsFile);

  if not result then
  begin
    infoln(FCrossModuleName+ ': failed: searched binutil '+AsFile+' without results. ',etInfo);
    FAlreadyWarned:=true;
  end;

  if result then
  begin
    // Configuration snippet for FPC
    //http://wiki.freepascal.org/Setup_Cross_Compile_For_ARM#Make_FPC_able_to_cross_compile_for_arm-wince
    //adjusted by
    //http://wiki.freepascal.org/arm-wince
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath)+LineEnding+ {search this directory for compiler utilities}
    '-XP'+FBinUtilsPrefix+LineEnding+ {Prepend the binutils names}
    '-darm'+LineEnding+ {pass arm to linker}
    '-Twince'; {target operating system}
  end;
end;

constructor TWin32_wincearm.Create;
begin
  inherited Create;
  FCrossModuleName:='Win32_wincearm';
  FBinUtilsPrefix:='arm-wince-'; //search algorithm may modify this
  FBinUtilsPath:='';
  FFPCCFGSnippet:=''; //will be filled in later
  FLibsPath:='';
  FTargetCPU:='arm';
  FTargetOS:='wince';
  FAlreadyWarned:=false;
  infoln('TWin32_wincearm crosscompiler loading',etDebug);
end;

destructor TWin32_wincearm.Destroy;
begin
  inherited Destroy;
end;

{$IF (DEFINED (WIN32)) OR (DEFINED(WIN64))}
// Even though it's officially for Win32, win64 can run x86 binaries without problem, so allow it.
var
  Win32_wincearm:TWin32_wincearm;

initialization
  Win32_wincearm:=TWin32_wincearm.Create;
  RegisterExtension(Win32_wincearm.TargetCPU+'-'+Win32_wincearm.TargetOS,Win32_wincearm);
finalization
  Win32_wincearm.Destroy;
{$ENDIF}
end.

