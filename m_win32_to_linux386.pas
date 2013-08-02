unit m_win32_to_linux386;
{ Cross compiles from Windows 32 to Linux x86/32 bit
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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller,fpcuputil;

implementation
type

{ TWin32_Linux386 }
TWin32_Linux386 = class(TCrossInstaller)
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

{ TWin32_Linux386 }
function TWin32_Linux386.TargetSignature: string;
begin
  result:=FTargetCPU+'-'+TargetOS;
end;

function TWin32_Linux386.GetLibs(Basepath:string): boolean;
const
  DirName='i386-linux';
begin
//todo add support for separate cross dire
  // Using crossfpc directory naming
  FLibsPath:='lib\'+DirName;
  result:=DirectoryExists(IncludeTrailingPathDelimiter(BasePath)+FLibsPath);
  if not result then
  begin
    infoln('TWin32_Linux386: failed: searched libspath '+FLibsPath,etDebug);
    FLibsPath:=IncludeTrailingPathDelimiter(BasePath)+'..\cross\lib\'+DirName;
    result:=DirectoryExists(FLibsPath);
    if not result then
      infoln('TWin32_Linux386: failed: searched libspath '+FLibsPath,etDebug);
  end;
  if result then
    infoln('TWin32_Linux386: found libspath '+FLibsPath,etDebug);
end;

function TWin32_Linux386.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least
  result:=true;
end;

function TWin32_Linux386.GetBinUtils(Basepath:string): boolean;
const
  DirName='i386-linux';
var
  AsFile: string;
begin
  AsFile:=FBinUtilsPrefix+'as.exe';
  // Using crossfpc directory naming
  FBinUtilsPath:=IncludeTrailingPathDelimiter(BasePath)+'bin'+DirectorySeparator+DirName;
  result:=FileExists(FBinUtilsPath+DirectorySeparator+AsFile);
  if not result then
  begin
    infoln('TWin32_Linux386: failed: searched binutil '+AsFile+' in directory '+FBinUtilsPath,etDebug);
    //todo: fix fallback to separate dir; use real argument from command line to control it
    FBinUtilsPath:=ExpandFileName(IncludeTrailingPathDelimiter(BasePath)+'..\cross\bin\'+DirName);
    result:=FileExists(FBinUtilsPath+DirectorySeparator+AsFile);
    if not result then
      infoln('TWin32_Linux386: failed: searched binutil '+AsFile+' in directory '+FBinUtilsPath,etDebug);
  end;
  if result then
    infoln('TWin32_Linux386: found binutil '+AsFile+' in directory '+FBinUtilsPath,etDebug);
end;

constructor TWin32_Linux386.Create;
begin
  inherited Create;
  FBinUtilsPrefix:='i386-linux-'; //crossfpc nomenclature
  FBinUtilsPath:='';
  FLibsPath:='';;
  FTargetCPU:='i386';
  FTargetOS:='linux';
  FAlreadyWarned:=false;
  infoln('TWin32_Linux386 crosscompiler loading',etDebug);
end;

destructor TWin32_Linux386.Destroy;
begin
  inherited Destroy;
end;

var
  Win32_Linux386:TWin32_Linux386;

{$IF (DEFINED (WIN32)) OR (DEFINED(WIN64))}
// Even though it's officially for Win32, win64 can run x86 binaries without problem, so allow it.
initialization
  Win32_Linux386:=TWin32_Linux386.Create;
  RegisterExtension(Win32_Linux386.TargetCPU+'-'+Win32_Linux386.TargetOS,Win32_Linux386);
finalization
  Win32_Linux386.Destroy;
{$ENDIF}
end.

