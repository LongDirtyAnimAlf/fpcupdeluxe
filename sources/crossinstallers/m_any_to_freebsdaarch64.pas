unit m_any_to_freebsdaarch64;

{ Cross compiles from e.g. Linux 64 bit (or any other OS with relevant binutils/libs) to FreeBSD aarch64
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
  Classes, SysUtils;

const
  MAXFREEBSDVERSION=13;
  MINFREEBSDVERSION=11;

implementation

uses
  FileUtil, m_crossinstaller;

type

{ Tany_freebsdaarch64 }
Tany_freebsdaarch64 = class(TCrossInstaller)
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

{ Tany_freebsdaarch64 }

function Tany_freebsdaarch64.GetLibs(Basepath:string): boolean;
var
  aVersion:integer;
begin
  result:=inherited;
  if result then exit;

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LIBCFILENAME);

  // first search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,LIBCFILENAME);

  if not result then
  begin
    // look for versioned libraries
    for aVersion:=13 downto 7 do
    begin
      result:=SimpleSearchLibrary(BasePath,DirName+InttoStr(aVersion),LIBCFILENAME);
      if result then break;
    end;
  end;

  if not result then
  begin
    {$IFDEF UNIX}
    FLibsPath:='/usr/lib/aarch64-freebsd-gnu'; //debian Jessie+ convention
    result:=DirectoryExists(FLibsPath);
    if not result then
    ShowInfo('Searched but not found libspath '+FLibsPath);
    {$ENDIF}
  end;

  SearchLibraryInfo(result);
  if result then
  begin
    FLibsFound:=True;
    AddFPCCFGSnippet('-Xd'); {buildfaq 3.4.1 do not pass parent /lib etc dir to linker}
    AddFPCCFGSnippet('-Fl'+LibsPath); {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    //AddFPCCFGSnippet('-XR'+LibsPath); {buildfaq 1.6.4/3.3.1: the directory to look for the target libraries ... just te be safe ...}
    AddFPCCFGSnippet('-Xr/usr/lib');
  end;
end;

{$ifndef FPCONLY}
function Tany_freebsdaarch64.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least
  result:=inherited;
end;
{$endif}

function Tany_freebsdaarch64.GetBinUtils(Basepath:string): boolean;
var
  AsFile: string;
  AsDirectory: string;
  BinPrefixTry: string;
  i:integer;
begin
  result:=inherited;
  if result then exit;

  AsFile:=BinUtilsPrefix+ASFILENAME+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  if (not result) then
  begin
    // look for versioned binutils
    BinPrefixTry:=FBinUtilsPrefix;
    SetLength(BinPrefixTry,Length(BinPrefixTry)-1);
    for i:=MAXFREEBSDVERSION downto MINFREEBSDVERSION do
    begin
      AsFile:=BinPrefixTry+InttoStr(i)+'-'+ASFILENAME+GetExeExt;
      result:=SearchBinUtil(BasePath,AsFile);
      if not result then
        result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
      if result then
      begin
        FBinUtilsPrefix:=BinPrefixTry+InttoStr(i)+'-';
        break;
      end;
    end;
  end;

  if (not result) then
  begin
    // look for binutils in versioned directories
    AsFile:=BinUtilsPrefix+ASFILENAME+GetExeExt;
    i:=MAXFREEBSDVERSION;
    while (i>=MINFREEBSDVERSION) do
    begin
      if i=MINFREEBSDVERSION then
        AsDirectory:=DirName
      else
        AsDirectory:=DirName+InttoStr(i);
      result:=SimpleSearchBinUtil(BasePath,AsDirectory,AsFile);
      if not result then
      begin
        // Also allow for (cross)binutils without prefix
        result:=SimpleSearchBinUtil(BasePath,AsDirectory,ASFILENAME+GetExeExt);
        if result then FBinUtilsPrefix:=''
      end;
      if result then break;
      Dec(i);
    end;
  end;

  SearchBinUtilsInfo(result);

  if result then
  begin
    FBinsFound:=true;
    // Configuration snippet for FPC
    AddFPCCFGSnippet('-FD'+BinUtilsPath); {search this directory for compiler utilities}
    AddFPCCFGSnippet('-XP'+BinUtilsPrefix); {Prepend the binutils names}
  end;
end;

constructor Tany_freebsdaarch64.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.aarch64;
  FTargetOS:=TOS.freebsd;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor Tany_freebsdaarch64.Destroy;
begin
  inherited Destroy;
end;

var
  any_freebsdaarch64:Tany_freebsdaarch64;

initialization
  any_freebsdaarch64:=Tany_freebsdaarch64.Create;
  RegisterCrossCompiler(any_freebsdaarch64.RegisterName,any_freebsdaarch64);

finalization
  any_freebsdaarch64.Destroy;

end.

