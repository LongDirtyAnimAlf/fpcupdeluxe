unit m_any_to_linuxaarch64;

{ Cross compiles from e.g. Linux 64 bit (or any other OS with relevant binutils/libs) to Linux 64 bit aarch
Copyright (C) 2014 Reinier Olislagers

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
  Classes, SysUtils, m_crossinstaller,fpcuputil,fileutil;

implementation
const
  CrossModuleName='Tany_linuxaarch64';

type

{ Tany_linuxaarch64 }
Tany_linuxaarch64 = class(TCrossInstaller)
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

{ Tany_linuxaarch64 }
function Tany_linuxaarch64.TargetSignature: string;
begin
  result:=FTargetCPU+'-'+TargetOS;
end;

function Tany_linuxaarch64.GetLibs(Basepath:string): boolean;
const
  DirName='aarch64-linux';
  LibName='libc.so';
begin
  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LibName);

  // first search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName);

  if not result then
  begin
    {$IFDEF UNIX}
    FLibsPath:='/usr/lib/aarch64-linux-gnu'; //debian Jessie+ convention
    result:=DirectoryExists(FLibsPath);
    if not result then
    infoln('Tany_linuxaarch64: failed: searched libspath '+FLibsPath,etInfo);
    {$ENDIF}
  end;

  SearchLibraryInfo(result);
  if result then
  begin
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Fl'+IncludeTrailingPathDelimiter(FLibsPath)+LineEnding+ {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    '-Xr/usr/lib';//+LineEnding+ {buildfaq 3.3.1: makes the linker create the binary so that it searches in the specified directory on the target system for libraries}
    //'-FL/usr/lib/ld-linux.so.2' {buildfaq 3.3.1: the name of the dynamic linker on the target};
  end;
end;

{$ifndef FPCONLY}
function Tany_linuxaarch64.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least
  result:=true;
end;
{$endif}

function Tany_linuxaarch64.GetBinUtils(Basepath:string): boolean;
const
  DirName='aarch64-linux';
var
  AsFile: string;
begin
  inherited;

  AsFile:=FBinUtilsPrefix+'as'+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  // Also allow for (cross)binutils without prefix
  if not result then
  begin
    FBinUtilsPrefix:='';
    AsFile:=FBinUtilsPrefix+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
  end;

  {$IFDEF UNIX}
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
  {$ENDIF}

  SearchBinUtilsInfo(result);
  if result then
  begin
    // Configuration snippet for FPC
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath)+LineEnding+ {search this directory for compiler utilities}
    '-XP'+FBinUtilsPrefix+LineEnding {Prepend the binutils names};
  end;
end;

constructor Tany_linuxaarch64.Create;
begin
  inherited Create;
  FCrossModuleName:='any_linuxaarch64';
  FBinUtilsPrefix:='aarch64-linux-';
  FBinUtilsPath:='';
  FFPCCFGSnippet:='';
  FLibsPath:='';
  FTargetCPU:='aarch64';
  FTargetOS:='linux';
  FAlreadyWarned:=false;
  infoln('Tany_linuxaarch64 crosscompiler loading',etDebug);
end;

destructor Tany_linuxaarch64.Destroy;
begin
  inherited Destroy;
end;

var
  any_linuxaarch64:Tany_linuxaarch64;

initialization
  any_linuxaarch64:=Tany_linuxaarch64.Create;
  RegisterExtension(any_linuxaarch64.TargetCPU+'-'+any_linuxaarch64.TargetOS,any_linuxaarch64);
finalization
  any_linuxaarch64.Destroy;

end.

