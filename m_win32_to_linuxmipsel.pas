unit m_win32_to_linuxmipsel;
{ Cross compiles from Windows 32 to mipsel 32 bit
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
Tested with Sourcery CodeBench Lite GNU/Linux
http://www.mentor.com/embedded-software/sourcery-tools/sourcery-codebench/editions/lite-edition/mips-gnu-linux
e.g. mips-2013.05-36-mips-linux-gnu.exe
See page 15 of the getting started manual for the layout of lib and relation to architecture/gcc compiler options

- Adapt (add) for other setups
- Note that the libs may not match your actual system. If so, replace them
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller,fpcuputil;

implementation
type

{ Twin32_linuxmipsel }
Twin32_linuxmipsel = class(TCrossInstaller)
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

{ Twin32_linuxmipsel }
function Twin32_linuxmipsel.TargetSignature: string;
begin
  result:=FTargetCPU+'-'+TargetOS;
end;

function Twin32_linuxmipsel.GetLibs(Basepath:string): boolean;
const
  DirName='mipsel-linux';
begin
//todo add support for separate cross dire  
  FLibsPath:='lib\'+DirName;
  result:=DirectoryExists(IncludeTrailingPathDelimiter(BasePath)+FLibsPath);
  if not result then
  begin
    // Show path info etc so the user can fix his setup if errors occur
    infoln('Twin32_linuxmipsel: failed: searched libspath '+FLibsPath,etInfo);
    FLibsPath:=ExpandFileName(IncludeTrailingPathDelimiter(BasePath)+'..\cross\lib\'+DirName);
    result:=DirectoryExists(FLibsPath);
    if not result then
      infoln('Twin32_linuxmipsel: failed: searched libspath '+FLibsPath,etInfo);
  end;
  if result then
  begin
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Fl'+IncludeTrailingPathDelimiter(FLibsPath) {buildfaq 3.3.1:  the directory to look for the target  libraries};
    infoln('Twin32_linuxmipsel: found libspath '+FLibsPath,etInfo);
  end;
end;

function Twin32_linuxmipsel.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least
  result:=true;
end;

function Twin32_linuxmipsel.GetBinUtils(Basepath:string): boolean;
const
  DirName='mipsel-linux';
var
  AsFile: string;
begin
  AsFile:=FBinUtilsPrefix+'as.exe';  
  FBinUtilsPath:=IncludeTrailingPathDelimiter(BasePath)+'bin'+DirectorySeparator+DirName;
  result:=FileExists(FBinUtilsPath+DirectorySeparator+AsFile);
  if not result then
  begin
    // Show path info etc so the user can fix his setup if errors occur
    infoln('Twin32_linuxmipsel: failed: searched binutil '+AsFile+' in directory '+FBinUtilsPath,etInfo);
    //todo: fix fallback to separate dir; use real argument from command line to control it
    FBinUtilsPath:=ExpandFileName(IncludeTrailingPathDelimiter(BasePath)+'..\cross\bin\'+DirName);
    result:=FileExists(FBinUtilsPath+DirectorySeparator+AsFile);
    if not result then
      infoln('Twin32_linuxmipsel: failed: searched binutil '+AsFile+' in directory '+FBinUtilsPath,etInfo);
  end;
  if result then
  begin
    // Configuration snippet for FPC
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath)+LineEnding+ {search this directory for compiler utilities}
    '-XP'+FBinUtilsPrefix+LineEnding+ {Prepend the binutils names}
    '-Tlinux'; {target operating system}
    infoln('Twin32_linuxmipsel: found binutil '+AsFile+' in directory '+FBinUtilsPath,etInfo);
  end;
end;

constructor Twin32_linuxmipsel.Create;
begin
  inherited Create;
  FBinUtilsPrefix:='mipsel-linux-';
  FBinUtilsPath:='';
  FFPCCFGSnippet:='';
  FLibsPath:='';
  FTargetCPU:='mipsel';
  FTargetOS:='linux';
  FAlreadyWarned:=false;
  infoln('Twin32_linuxmipsel crosscompiler loading',etDebug);
end;

destructor Twin32_linuxmipsel.Destroy;
begin
  inherited Destroy;
end;

var
  Win32_linuxmipsel:Twin32_linuxmipsel;

{$IFDEF MSWINDOWS)}
// Even though it's officially for x86, x64 may work
initialization
  Win32_linuxmipsel:=Twin32_linuxmipsel.Create;
  RegisterExtension(Win32_linuxmipsel.TargetCPU+'-'+Win32_linuxmipsel.TargetOS,Win32_linuxmipsel);
finalization
  Win32_linuxmipsel.Destroy;
{$ENDIF}
end.

