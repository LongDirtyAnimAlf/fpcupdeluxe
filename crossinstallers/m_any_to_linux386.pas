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
  Classes, SysUtils;

implementation

uses
  Process, FileUtil, m_crossinstaller, fpcuputil;

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
  NormalDirName='i386-linux';
  MUSLDirName='i386-musllinux';
var
  aDirName,aLibName,s:string;
begin
  result:=FLibsFound;
  if result then exit;

  if FMUSL then
  begin
    aDirName:=MUSLDirName;
    aLibName:='libc.musl-'+GetCPU(TargetCPU)+'.so.1';
  end
  else
  begin
    aDirName:=NormalDirName;
    aLibName:=LIBCNAME;
  end;

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,aLibName);

  // first search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,aDirName,aLibName);

  if result then
  begin
    FLibsFound:=True;
    AddFPCCFGSnippet('-Xd'); {buildfaq 3.4.1 do not pass parent /lib etc dir to linker}
    AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath)); {buildfaq 1.6.4/3.3.1: the directory to look for the target  libraries}
    //Remember: -XR sets the sysroot path used for linking
    //AddFPCCFGSnippet('-XR'+IncludeTrailingPathDelimiter(FLibsPath)+'lib64'); {buildfaq 1.6.4/3.3.1: the directory to look for the target libraries ... just te be safe ...}
    //Remember: -Xr adds a  rlink path to the linker
    AddFPCCFGSnippet('-Xr/usr/lib');

    if FMUSL then
    begin
      aLibName:='ld-musl-'+GetCPU(TargetCPU)+'.so.1';
      AddFPCCFGSnippet('-FL/lib/'+aLibName);
    end;
  end;

  if not result then
  begin
    {$IFDEF LINUX}
    {$IFDEF MULTILIB}
    FLibsPath:='/usr/lib/i386-linux-gnu'; //debian (multilib) Jessie+ convention
    result:=DirectoryExists(FLibsPath);
    if result then
    begin
      FLibsFound:=True;
      AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath));
      {$ifdef CPU64}

      s:='/lib32';
      if DirectoryExists(s) then
      begin
        s:=s+DirectorySeparator;
        AddFPCCFGSnippet('-Fl'+s);
      end;

      s:='/usr/lib32';
      if DirectoryExists(s) then
      s:=s+DirectorySeparator;
      AddFPCCFGSnippet('-Fl'+s);

      s:='/lib/i386-linux-gnu';
      if DirectoryExists(s) then
      s:=s+DirectorySeparator;
      AddFPCCFGSnippet('-Fl'+s);

      // gcc 32bit multilib
      s:=IncludeTrailingPathDelimiter(GetStartupObjects)+'32';
      if DirectoryExists(s) then
      begin
        AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(s));
      end;
      {$endif}
    end else ShowInfo('Searched but not found (multilib) libspath '+FLibsPath);
    {$ENDIF MULTILIB}
    {$ENDIF LINUX}
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
  NormalDirName='i386-linux';
  MUSLDirName='i386-musllinux';
var
  AsFile: string;
  BinPrefixTry: string;
  aDirName: string;
  s:string;
begin
  result:=inherited;
  if result then exit;

  if FMUSL then
    aDirName:=MUSLDirName
  else
    aDirName:=NormalDirName;

  AsFile:=FBinUtilsPrefix+'as'+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);

  {$IFDEF MULTILIB}
    {$IFDEF CPUX64}
    // Now also allow for empty binutilsprefix in the right directory:
    if (NOT result) then
    begin
      RunCommand('objdump',['-i'], s,[poUsePipes, poStderrToOutPut],swoHide);
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
    {$IFDEF CPUX86}
    // Now also allow for empty binutilsprefix in the right directory:
    if (NOT result) then
    begin
      RunCommand('objdump',['-i'], s,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
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
    result:=SimpleSearchBinUtil(BasePath,aDirName,AsFile);

  // Also allow for (cross)binutils without prefix
  if not result then
  begin
    BinPrefixTry:='';
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,aDirName,AsFile);
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
  FTargetCPU:=TCPU.i386;
  FTargetOS:=TOS.linux;
  Reset;
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
  RegisterCrossCompiler(any_linux386.RegisterName,any_linux386);

finalization
  any_linux386.Destroy;

end.

