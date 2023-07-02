unit m_any_to_linuxloongarch64;

{ Cross compiles from any (or any other OS with relevant binutils/libs) to loongarch64 linux
Copyright (C) 2023 Don

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

implementation

uses
  FileUtil, fpcuputil, m_crossinstaller;

type

{ Tany_linuxloongarch64 }
Tany_linuxloongarch64 = class(TCrossInstaller)
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

{ Tany_linuxloongarch64 }

function Tany_linuxloongarch64.GetLibs(Basepath:string): boolean;
const
  LINKER1='ld.so.1';
  LINKER2='ld-linux-loongarch-lp64d.so.1';
var
  DynLinker:string;
begin
  result:=inherited;

  if result then exit;

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LIBCFILENAME);

  // first search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,LIBCFILENAME);

  SearchLibraryInfo(result);

  if result then
  begin
    FLibsFound:=True;
    AddFPCCFGSnippet('-Xd'); // do not pass parent /lib etc dir to linker}
    AddFPCCFGSnippet('-Fl'+LibsPath);
    DynLinker:='';
    if FileExists(ConcatPaths([FLibsPath,LINKER1])) then
    begin
      AddFPCCFGSnippet('-Xr/lib64');
      DynLinker:='/lib64/'+LINKER1;
    end;
    if FileExists(ConcatPaths([FLibsPath,LINKER2])) then
    begin
      AddFPCCFGSnippet('-Xr/usr/lib64');
      DynLinker:='/usr/lib64/'+LINKER2;
    end;
    if (Length(DynLinker)>0) then AddFPCCFGSnippet('-FL'+DynLinker,false);
  end;
end;

{$ifndef FPCONLY}
function Tany_linuxloongarch64.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least
  result:=inherited;
end;
{$endif}

function Tany_linuxloongarch64.GetBinUtils(Basepath:string): boolean;
var
  AsFile: string;
  BinPrefixTry: string;
begin
  result:=inherited;
  if result then exit;

  AsFile:=BinUtilsPrefix+ASFILENAME+GetExeExt;
  BinPrefixTry:=FBinUtilsPrefix;

  result:=SearchBinUtil(BasePath,AsFile);

  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  // Also allow for (cross)binutils with special prefix
  if not result then
  begin
    BinPrefixTry:=TargetCPUName+'-unknown-'+TargetOSName+'-gnu-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
  end;

  // Also allow for (cross)binutils without prefix in the right directory
  if (not result) then
  begin
    BinPrefixTry:='';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if (not result) then
      result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
  end;

  if result then FBinUtilsPrefix:=BinPrefixTry;

  SearchBinUtilsInfo(result);

  if result then
  begin
    FBinsFound:=true;
    AddFPCCFGSnippet('-FD'+BinUtilsPath);
    AddFPCCFGSnippet('-XP'+BinUtilsPrefix);
  end;
end;

constructor Tany_linuxloongarch64.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.loongarch64;
  FTargetOS:=TOS.linux;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor Tany_linuxloongarch64.Destroy;
begin
  inherited Destroy;
end;

var
  any_linuxloongarch64:Tany_linuxloongarch64;

initialization
  any_linuxloongarch64:=Tany_linuxloongarch64.Create;
  RegisterCrossCompiler(any_linuxloongarch64.RegisterName,any_linuxloongarch64);

finalization
  any_linuxloongarch64.Destroy;

end.

