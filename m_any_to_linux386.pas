unit m_any_to_linux386;

{ Cross compiles from e.g. Linux 64 bit (or any other OS with relevant binutils/libs) to Linux 32 bit
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

{
Debian: adding i386 libs/architecture support on e.g. x64 system
dpkg --add-architecture i386

Adapt (add) for other setups
}

{$mode objfpc}{$H+}

{$DEFINE MULTILIB}

interface

uses
  Classes, SysUtils, m_crossinstaller, fileutil;

implementation

uses
  processutils,fpcuputil;

type

{ Tany_linux386 }
Tany_linux386 = class(TCrossInstaller)
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

{ Tany_linux386 }

function Tany_linux386.GetLibs(Basepath:string): boolean;
const
  DirName='i386-linux';
var
  s:string;
  i:integer;
begin
  result:=FLibsFound;
  if result then exit;

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LIBCNAME);

  // first search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,LIBCNAME);

  if result then
  begin
    FLibsFound:=True;
    AddFPCCFGSnippet('-Xd'); {buildfaq 3.4.1 do not pass parent /lib etc dir to linker}
    AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath)); {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    //AddFPCCFGSnippet('-XR'+ExcludeTrailingPathDelimiter(FLibsPath)); {buildfaq 1.6.4/3.3.1: the directory to look for the target libraries ... just te be safe ...}
    AddFPCCFGSnippet('-Xr/usr/lib');
  end;

  if not result then
  begin
    {$IFDEF UNIX}
    {$IFDEF MULTILIB}
    FLibsPath:='/usr/lib/i386-linux-gnu'; //debian (multilib) Jessie+ convention
    result:=DirectoryExists(FLibsPath);
    if (NOT result) then
    begin
      FLibsPath:='/lib32';
      result:=DirectoryExists(FLibsPath);
    end;
    if result then
    begin
      FLibsFound:=True;
      AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath));
      //AddFPCCFGSnippet('-FL'+IncludeTrailingPathDelimiter(FLibsPath)+'ld-linux.so.2');
      {$ifdef CPU64}
      s:='/usr/lib32';
      if DirectoryExists(s) then
      begin
        AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(s));
      end;
      // gcc 32bit multilib
      s:=IncludeTrailingPathDelimiter(GetStartupObjects)+'32';
      if DirectoryExists(s) then
      begin
        AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(s));
      end;
      // set linker target; multilib //
      // we could test multilib by asking the linker ; ld --target-help, and process the output to see if elf32-i386 is supported
      //AddFPCCFGSnippet('-k-b elf32-i386'));
      {$endif}
      //AddFPCCFGSnippet('-FL/lib/ld-linux.so.2');
      {buildfaq 3.3.1: makes the linker create the binary so that it searches in the specified directory on the target system for libraries}
      //AddFPCCFGSnippet('-Xr'+ExcludeTrailingPathDelimiter(FLibsPath));
      //AddFPCCFGSnippet('-Xr/usr/lib);
    end else ShowInfo('Searched but not found (multilib) libspath '+FLibsPath);
    {$ENDIF}
    {$ENDIF}
  end;

  SearchLibraryInfo(result);
end;

{$ifndef FPCONLY}
function Tany_linux386.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least
  result:=inherited;
end;
{$endif}

function Tany_linux386.GetBinUtils(Basepath:string): boolean;
const
  DirName='i386-linux';
var
  AsFile: string;
  BinPrefixTry: string;
  s:string;
begin
  result:=inherited;
  if result then exit;

  AsFile:=FBinUtilsPrefix+'as'+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);

  {$IFDEF MULTILIB}
    {$IFDEF CPUX64}
    // Now also allow for empty binutilsprefix in the right directory:
    if (NOT result) then
    begin
      ExecuteCommand('objdump -i', s, False);
      if AnsiPos('elf32-i386', s) <> 0 then
      begin
        s:=Which('objdump');
        s:=ExtractFileDir(s);
        BinPrefixTry:='';
        AsFile:=BinPrefixTry+'as'+GetExeExt;
        // search local default cross-utils paths
        result:=SearchBinUtil(s,AsFile);
        if result then FBinUtilsPrefix:=BinPrefixTry;
      end;
    end;
    {$ENDIF}
    {$IFDEF CPUX32}
    // Now also allow for empty binutilsprefix in the right directory:
    if (NOT result) then
    begin
      ExecuteCommand('objdump -i', s, False);
      if AnsiPos('elf64-x86-64', s) <> 0 then
      begin
        s:=Which('objdump');
        s:=ExtractFileDir(s);
        BinPrefixTry:='';
        AsFile:=BinPrefixTry+'as'+GetExeExt;
        // search local default cross-utils paths
        result:=SearchBinUtil(s,AsFile);
        if result then FBinUtilsPrefix:=BinPrefixTry;
      end;
    end;
  {$ENDIF}
  {$ENDIF}

  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

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
    // Configuration snippet for FPC
    AddFPCCFGSnippet('-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath));
    AddFPCCFGSnippet('-XP'+FBinUtilsPrefix);
  end;
end;

constructor Tany_linux386.Create;
begin
  inherited Create;
  FBinUtilsPrefix:='i386-linux-';
  FBinUtilsPath:='';
  FFPCCFGSnippet:='';
  FLibsPath:='';
  FTargetCPU:='i386';
  FTargetOS:='linux';
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor Tany_linux386.Destroy;
begin
  inherited Destroy;
end;

var
  any_linux386:Tany_linux386;

initialization
  any_linux386:=Tany_linux386.Create;
  RegisterExtension(any_linux386.TargetCPU+'-'+any_linux386.TargetOS,any_linux386);
finalization
  any_linux386.Destroy;

end.

