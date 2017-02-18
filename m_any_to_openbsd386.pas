unit m_any_to_openbsd386;

{ Cross compiles from any platform with correct binutils to openbsd i386
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
  Classes, SysUtils, m_crossinstaller, fileutil;

implementation

const
  ARCH='i386';
  OS='openbsd';

type

{ TAny_OpenBSD386 }
TAny_OpenBSD386 = class(TCrossInstaller)
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

{ TAny_OpenBSD386 }

function TAny_OpenBSD386.GetLibs(Basepath:string): boolean;
const
  DirName=ARCH+'-'+OS;
  LibName='libc.so.88.0';
begin

  result:=FLibsFound;
  if result then exit;

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LibName);

  // first search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,LibName);

  SearchLibraryInfo(result);
  if result then
  begin
    FLibsFound:=true;
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    //todo: implement -Xr for other platforms if this setup works
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
      '-Xd'+LineEnding+ {buildfaq 3.4.1 do not pass parent /lib etc dir to linker}
      '-Fl'+IncludeTrailingPathDelimiter(FLibsPath)+LineEnding+ {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
      '-XR'+IncludeTrailingPathDelimiter(FLibsPath)+LineEnding+
      '-k--allow-shlib-undefined'+LineEnding+
      '-k--allow-multiple-definition'+LineEnding+
      '-Xr/usr/lib'; {buildfaq 3.3.1: makes the linker create the binary so that it searches in the specified directory on the target system for libraries}
  end
  else
  begin
    ShowInfo('For simple programs that do not call (C) libraries, this is not necessary. However, you MAY want to copy your /usr/lib from your AIX machine to your cross lib directory.');
  end;
  result:=true; //this step is optional at least for simple hello world programs
end;

{$ifndef FPCONLY}
function TAny_OpenBSD386.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least, add to FFPCCFGSnippet
  ShowInfo('Implement lcl libs path from basepath '+BasePath+' for platform '+LCL_Platform,etDebug);
  result:=inherited;
end;
{$endif}

function TAny_OpenBSD386.GetBinUtils(Basepath:string): boolean;
const
  DirName=ARCH+'-'+OS;
var
  AsFile: string;
  BinPrefixTry: string;
begin
  result:=inherited;
  if result then exit;

  // Start with any names user may have given
  AsFile:=FBinUtilsPrefix+'as'+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  // Also allow for crossfpc naming
  if not result then
  begin
    BinPrefixTry:=ARCH+'-'+OS+'-';
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  // Also allow for crossbinutils without prefix
  if not result then
  begin
    BinPrefixTry:='';
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  SearchBinUtilsInfo(result);

  if not result then
  begin
    ShowInfo('Suggestion for cross binutils: please check http://wiki.lazarus.freepascal.org/FPC_AIX_Port.');
    FAlreadyWarned:=true;
  end
  else
  begin
    FBinsFound:=true;
    // Configuration snippet for FPC
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath)+LineEnding+ {search this directory for compiler utilities}
    '-XP'+FBinUtilsPrefix; {Prepend the binutils names}
  end;
end;

constructor TAny_OpenBSD386.Create;
begin
  inherited Create;
  FTargetCPU:=ARCH;
  FTargetOS:=OS;
  FBinUtilsPrefix:=ARCH+'-'+OS+'-';
  FBinUtilsPath:='';
  FCompilerUsed:=ctBootstrap;
  FFPCCFGSnippet:=''; //will be filled in later
  FLibsPath:='';
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor TAny_OpenBSD386.Destroy;
begin
  inherited Destroy;
end;

var
  Any_OpenBSD386:TAny_OpenBSD386;

initialization
  Any_OpenBSD386:=TAny_OpenBSD386.Create;
  RegisterExtension(Any_OpenBSD386.TargetCPU+'-'+Any_OpenBSD386.TargetOS,Any_OpenBSD386);
finalization
  Any_OpenBSD386.Destroy;
end.

