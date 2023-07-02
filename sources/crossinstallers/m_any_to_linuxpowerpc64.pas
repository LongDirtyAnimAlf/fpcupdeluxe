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
  Classes, SysUtils;

implementation

uses
  FileUtil, m_crossinstaller, fpcuputil;

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
    if (StringListStartsWith(FCrossOpts,'-Cb-')<>-1) then
    begin
      // we have little endian !!
      result:=SimpleSearchLibrary(BasePath,'powerpc64le-linux',LIBCFILENAME);
    end;
  end;

  if not result then
  begin
    {$IFDEF UNIX}
    {$IFDEF MULTILIB}
    FLibsPath:='/usr/lib/powerpc64-linux-gnu'; //debian Jessie+ convention
    result:=DirectoryExists(FLibsPath);
    if not result then
    begin
      if (StringListStartsWith(FCrossOpts,'-Cb-')<>-1) then
      begin
        // we have little endian !!
        FLibsPath:='/usr/lib/powerpc64le-linux-gnu'; //debian Jessie+ convention
        result:=DirectoryExists(FLibsPath);
      end;
    end;
    {$ENDIF}
    {$ENDIF}
  end;

  SearchLibraryInfo(result);

  if result then
  begin
    FLibsFound:=True;
    AddFPCCFGSnippet('-Fl'+LibsPath);
    AddFPCCFGSnippet('-Xr/usr/lib');
  end;

  if not result then
  begin
    if (StringListStartsWith(FCrossOpts,'-Cb-')<>-1) then
    begin
      // we have little endian libs: get them !!
      ShowInfo('Option "-Cb-" detected: trying to get the PowerPC64 little endian libs.',etInfo);
    end
    else
    begin
      //no big endian libs yet: go on without them
      ShowInfo('Libspath ignored; it is optional for this cross compiler.',etInfo);
      FLibsPath:='';
      result:=true;
    end;
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
var
  AsFile: string;
  BinPrefixTry: string;
  //aOption:string;
  //i:integer;
begin
  result:=inherited;
  if result then exit;

  AsFile:=BinUtilsPrefix+ASFILENAME+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);

  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  if (not result) then
  begin
    BinPrefixTry:='powerpc64-linux-gnu-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;

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
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  SearchBinUtilsInfo(result);

  if result then
  begin
    FBinsFound:=true;

    AddFPCCFGSnippet('-FD'+BinUtilsPath);
    AddFPCCFGSnippet('-XP'+BinUtilsPrefix);

    (*
    2017-10-19 FPC trunk
    from t_linux.pas powerpc64 part of the FPC compiler:
    ***************
      if (target_info.abi=abi_powerpc_elfv2) and
         (target_info.endian=endian_little) then
        platformopt:=' -b elf64-powerpcle -m elf64lppc'
      else
        platformopt:=' -b elf64-powerpc -m elf64ppc';
    ***************
    So, we only get little endian from the linker if we use ABI elfv2 AND set little_endian
    *)

    {
    // see above : new abi - use it !!
    i:=StringListStartsWith(FCrossOpts,'-Ca');
    if i=-1 then
    begin
      aOption:='-Caelfv2';
      AddCrossOption(aOption);
      ShowInfo('Did not find any -Ca ABI parameter; using '+aOption+'.',etInfo);
    end else aOption:=Trim(FCrossOpts[i]);
    AddFPCCFGSnippet(aOption);

    // see above : default to Little Endian
    i:=StringListStartsWith(FCrossOpts,'-Cb');
    if i=-1 then
    begin
      aOption:='-Cb-';
      AddCrossOption(aOption);
      ShowInfo('Did not find any -Cb endianess parameter; using '+aOption+' (little endian).',etInfo);
    end
    else aOption:=Trim(FCrossOpts[i]);
    AddFPCCFGSnippet(aOption);
    }

  end;
end;

constructor Tany_linuxpowerpc64.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.powerpc64;
  FTargetOS:=TOS.linux;
  Reset;
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
  RegisterCrossCompiler(any_linuxpowerpc64.RegisterName,any_linuxpowerpc64);

finalization
  any_linuxpowerpc64.Destroy;

end.

