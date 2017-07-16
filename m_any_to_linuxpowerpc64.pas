unit m_any_to_linuxpowerpc64;

{ Cross compiles from any (or any other OS with relevant binutils/libs) to Linux 64 bit
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
  Classes, SysUtils, m_crossinstaller, fileutil, fpcuputil;

implementation

type

{ Tany_linuxpowerpc64 }
Tany_linuxpowerpc64 = class(TCrossInstaller)
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

{ Tany_linuxpowerpc64 }

function Tany_linuxpowerpc64.GetLibs(Basepath:string): boolean;
const
  DirName='powerpc64-linux';
  LibName='libc.so';
begin
  FLibsFound:=true;
  result:=FLibsFound;
  if result then exit;

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LibName);

  // first search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,LibName);

  if not result then
  begin
    {$IFDEF UNIX}
    {$IFDEF MULTILIB}
    FLibsPath:='/usr/lib/x86_64-linux-gnu'; //debian Jessie+ convention
    result:=DirectoryExists(FLibsPath);
    if not result then
    ShowInfo('Searched but not found libspath '+FLibsPath);
    {$ENDIF}
    {$ENDIF}
  end;

  SearchLibraryInfo(result);

  if result then
  begin
    FLibsFound:=True;
    AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath));
    AddFPCCFGSnippet('-Xr/usr/lib');
  end;
end;

{$ifndef FPCONLY}
function Tany_linuxpowerpc64.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least
  result:=inherited;
end;
{$endif}

function Tany_linuxpowerpc64.GetBinUtils(Basepath:string): boolean;
const
  DirName='powerpc64-linux';
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

  if (not result) then
  begin
    BinPrefixTry:='powerpc64-linux-gnu-';
    AsFile:=BinPrefixTry+'as'+GetExeExt;

    result:=SearchBinUtil(BasePath,AsFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

    // also check in the gnu directory
    if not result then
       result:=SimpleSearchBinUtil(BasePath,DirName+'-gnu',AsFile);

    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  // Also allow for (cross)binutils without prefix
  if not result then
  begin
    BinPrefixTry:='';
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  SearchBinUtilsInfo(result);

  if result then
  begin
    FBinsFound:=true;

    // new abi: use it !!
    if StringListStartsWith(FCrossOpts,'-Ca')=-1 then
    begin
      FCrossOpts.Add('-Caelfv2 ');
      ShowInfo('Did not find any -Ca ABI parameter; using -Caelfv2.',etInfo);
    end;

    // default to Little Endian
    if StringListStartsWith(FCrossOpts,'-Cb')=-1 then
    begin
      FCrossOpts.Add('-Cb- ');
      ShowInfo('Did not find any -Cb endianess parameter; using -Cb- (little endian).',etInfo);
    end;

    AddFPCCFGSnippet('-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath));
    AddFPCCFGSnippet('-XP'+FBinUtilsPrefix);
  end;
end;

constructor Tany_linuxpowerpc64.Create;
begin
  inherited Create;
  FBinUtilsPrefix:='powerpc64-linux-';
  FBinUtilsPath:='';
  FFPCCFGSnippet:='';
  FLibsPath:='';
  FTargetCPU:='powerpc64';
  FTargetOS:='linux';
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor Tany_linuxpowerpc64.Destroy;
begin
  inherited Destroy;
end;

var
  any_linuxpowerpc64:Tany_linuxpowerpc64;

initialization
  any_linuxpowerpc64:=Tany_linuxpowerpc64.Create;
  RegisterExtension(any_linuxpowerpc64.TargetCPU+'-'+any_linuxpowerpc64.TargetOS,any_linuxpowerpc64);
finalization
  any_linuxpowerpc64.Destroy;

end.

