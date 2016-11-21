unit m_any_to_darwinx64;

{ Cross compiles to Darwin 64 bit
Copyright (C) 2014 Reinier Olislagers / DonAlfredo

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
  Classes, SysUtils, m_crossinstaller, fpcuputil, fileutil;

implementation

type

{ Tany_darwinx64 }
Tany_darwinx64 = class(TCrossInstaller)
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

{ Tany_darwinx64 }
function Tany_darwinx64.TargetSignature: string;
begin
  result:=FTargetCPU+'-'+TargetOS;
end;

function Tany_darwinx64.GetLibs(Basepath:string): boolean;
const
  DirName='x86_64-darwin';
  LibName='libc.dylib';
begin

  result:=FLibsFound;
  if result then exit;

  {$ifdef MSWINDOWS}
  if Pos('osxcross',FBinUtilsPath)>0 then
  begin
    result:=true;
    if Pos('darwin13',FBinUtilsPrefix)>0 then FLibsPath:='C:\cygwin\opt\osxcross\target\SDK\MacOSX10.9.sdk\usr\lib';
    if Pos('darwin14',FBinUtilsPrefix)>0 then FLibsPath:='C:\cygwin\opt\osxcross\target\SDK\MacOSX10.10.sdk\usr\lib';
  end;
  {$endif}

  // begin simple: check presence of library file in basedir
  if not result then
    result:=SearchLibrary(Basepath,LibName);

  // first search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName);

  if not result then
  begin
    {$IFDEF UNIX}
    FLibsPath:='/usr/lib/x86_64-linux-gnu'; //debian Jessie+ convention
    result:=DirectoryExists(FLibsPath);
    if not result then
    infoln('Tany_darwinx64: failed: searched libspath '+FLibsPath,etInfo);
    {$ENDIF}
  end;

  SearchLibraryInfo(result);
  if result then
  begin
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Fl'+IncludeTrailingPathDelimiter(FLibsPath);
    if Pos('osxcross',FLibsPath)>0 then
    begin
      FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
      '-Fl'+IncludeTrailingPathDelimiter(FLibsPath)+'system\';
    end;
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Xr/usr/lib';//+LineEnding+ {buildfaq 3.3.1: makes the linker create the binary so that it searches in the specified directory on the target system for libraries}
    //'-FL/usr/lib/ld-linux.so.2' {buildfaq 3.3.1: the name of the dynamic linker on the target};
  end;
end;

{$ifndef FPCONLY}
function Tany_darwinx64.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  result:=true;
end;
{$endif}

function Tany_darwinx64.GetBinUtils(Basepath:string): boolean;
const
  DirName='x86_64-darwin';
var
  AsFile: string;
  BinPrefixTry: string;
begin
  result:=inherited;
  if result then exit;

  AsFile:=FBinUtilsPrefix+'as'+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  {$ifdef MSWINDOWS}
  // Also allow for (cross)binutils from https://github.com/tpoechtrager/osxcross
  // version 10.10
  if IsWindows64
     then BinPrefixTry:='x86_64-apple-darwin14-'
     else BinPrefixTry:='i386-apple-darwin14-';

  if not result then
  begin
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  if not result then
  begin
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil('C:\cygwin\opt\osxcross\target\bin\',AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  // version 10.9
  if IsWindows64
     then BinPrefixTry:='x86_64-apple-darwin13-'
     else BinPrefixTry:='i386-apple-darwin13-';

  if not result then
  begin
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  if not result then
  begin
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil('C:\cygwin\opt\osxcross\target\bin\',AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;
  {$endif}

  SearchBinUtilsInfo(result);

  if result then
  begin
    FBinsFound:=true;
    // Configuration snippet for FPC
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath)+LineEnding+ {search this directory for compiler utilities}
    //'-Xr/usr/lib';//+LineEnding+ {buildfaq 3.3.1: makes the linker create the binary so that it searches in the specified directory on the target system for libraries}
    '-XP'+FBinUtilsPrefix+LineEnding {Prepend the binutils names};
  end;
end;

constructor Tany_darwinx64.Create;
begin
  inherited Create;
  FCrossModuleName:='any_darwinx64';
  FBinUtilsPrefix:='x86_64-darwin-';
  FBinUtilsPath:='';
  FFPCCFGSnippet:='';
  FLibsPath:='';
  FTargetCPU:='x86_64';
  FTargetOS:='darwin';
  FAlreadyWarned:=false;
  infoln('Tany_darwinx64 crosscompiler loading',etDebug);
end;

destructor Tany_darwinx64.Destroy;
begin
  inherited Destroy;
end;

var
  any_darwinx64:Tany_darwinx64;

initialization
  any_darwinx64:=Tany_darwinx64.Create;
  RegisterExtension(any_darwinx64.TargetCPU+'-'+any_darwinx64.TargetOS,any_darwinx64);
finalization
  any_darwinx64.Destroy;

end.

