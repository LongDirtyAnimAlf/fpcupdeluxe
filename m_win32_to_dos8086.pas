unit m_win32_to_dos8086;
{ Cross compiles from Windows 32 to DOS on the Intel 8086 and higher processor
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
ftp://ftp.freepascal.org/pub/fpc/contrib/cross/mingw/binutils....?!?!
todo: fix this

Add a cross directory under the fpcup "root" installdir directory (e.g. c:\development\cross, and e.g. regular fpc sources in c:\development\fpc)
Then place the binaries in c:\development\cross\bin\8086-dos
Binaries include
the NASM assembler
the OpenWatcom linker WLINK 
the OpenWatcom WLIB tool

todo: figure out how to actually build the cross compiler!?
-Pi8086 -Tdos? -Tdos seems to be undefined
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller,fpcuputil;

implementation
type

{ TWin32_dos8086 }
TWin32_dos8086 = class(TCrossInstaller)
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

{ TWin32_dos8086 }
function TWin32_dos8086.TargetSignature: string;
begin
  result:=FTargetCPU+'-'+TargetOS;
end;

function TWin32_dos8086.GetLibs(Basepath:string): boolean;
const
  DirName='8086-dos';
begin
  // DOS8086 does not need libs by default, but user can add them.
  FLibsPath:=ExpandFileName(IncludeTrailingPathDelimiter(BasePath)+'lib\'+DirName);
  result:=DirectoryExists(IncludeTrailingPathDelimiter(BasePath)+FLibsPath);
  if not result then
  begin
    // Show path info etc so the user can fix his setup if errors occur
    infoln('TWin32_dos8086: failed: searched libspath '+FLibsPath,etInfo);
    FLibsPath:=ExpandFileName(IncludeTrailingPathDelimiter(BasePath)+'..\cross\lib\'+DirName);
    result:=DirectoryExists(FLibsPath);
    if not result then
      infoln('TWin32_dos8086: failed: searched libspath '+FLibsPath,etInfo);
  end;
  if result then
  begin
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Fl'+IncludeTrailingPathDelimiter(FLibsPath) {buildfaq 1.6.4/3.3.1:  the directory to look for the target  libraries};
    infoln('TWin32_dos8086: found libspath '+FLibsPath,etInfo);
  end;
  if not result then
  begin
    //libs path is optional; it can be empty
    infoln('TWin32_dos8086: libspath ignored; it is optional for this cross comipler.',etInfo);
    FLibsPath:='';
    result:=true;
  end;
end;

function TWin32_dos8086.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  infoln('TWin32_dos8086: no support for LCL platform '+LCL_Platform,etInfo);
  result:=true;
end;

function TWin32_dos8086.GetBinUtils(Basepath:string): boolean;
const
  DirName='8086-dos';
var
  AsFile: string;
begin
  //todo: do ftp download from ftp repo; check executables (a la checklcl linux function)
  AsFile:=FBinUtilsPrefix+'as.exe';
  // Using crossfpc directory naming
  FBinUtilsPath:=IncludeTrailingPathDelimiter(BasePath)+'bin'+DirectorySeparator+DirName;
  result:=FileExists(FBinUtilsPath+DirectorySeparator+AsFile);
  if not result then
  begin
    // Show path info etc so the user can fix his setup if errors occur
    infoln('TWin32_dos8086: failed: searched binutil '+AsFile+' in directory '+FBinUtilsPath,etInfo);
    //todo: fix fallback to separate dir; use real argument from command line to control it
    FBinUtilsPath:=ExpandFileName(IncludeTrailingPathDelimiter(BasePath)+'..\cross\bin\'+DirName);
    result:=FileExists(FBinUtilsPath+DirectorySeparator+AsFile);
    if not result then
      infoln('TWin32_dos8086: failed: searched binutil '+AsFile+' in directory '+FBinUtilsPath,etInfo);
  end;
  if result then
  begin
    // Configuration snippet for FPC
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath)+LineEnding+ {search this directory for compiler utilities}
    '-XP'+FBinUtilsPrefix+LineEnding; {Prepend the binutils names}
    infoln('TWin32_dos8086: found binutil '+AsFile+' in directory '+FBinUtilsPath,etInfo);
  end;
end;

constructor TWin32_dos8086.Create;
begin
  inherited Create;
  FBinUtilsPrefix:='8086-dos-';
  FBinUtilsPath:='';
  FFPCCFGSnippet:=''; //will be filled in later
  FLibsPath:='';
  FTargetCPU:='8086';
  FTargetOS:='dos';
  FAlreadyWarned:=false;
  infoln('TWin32_dos8086 crosscompiler loading',etDebug);
end;

destructor TWin32_dos8086.Destroy;
begin
  inherited Destroy;
end;

{$IF (DEFINED (WIN32)) OR (DEFINED(WIN64))}
// Even though it's officially for Win32, win64 can run x86 binaries without problem, so allow it.
var
  Win32_dos8086:TWin32_dos8086;

initialization
  Win32_dos8086:=TWin32_dos8086.Create;
  RegisterExtension(Win32_dos8086.TargetCPU+'-'+Win32_dos8086.TargetOS,Win32_dos8086);
finalization
  Win32_dos8086.Destroy;
{$ENDIF}
end.

