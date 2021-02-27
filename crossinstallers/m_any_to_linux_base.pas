unit m_any_to_linux_base;

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

{$IFDEF LINUX}
{$IF DEFINED(CPUX86_64) OR DEFINED(CPUX86)}
{$DEFINE MULTILIB}
{$ENDIF}
{$ENDIF LINUX}

interface

uses
  Classes, SysUtils, m_crossinstaller;

type
  Tany_linux = class(TCrossInstaller)
  private
    FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
  {$IFDEF MULTILIB}
    FMultilib:boolean;
    function CheckMultilib:boolean;
  protected
    function GetMultilibDir:string;virtual;
    function GetMultilibDirShort:string;virtual;
    function GetObjdumpOutput:string;virtual;
    function GetLDOutput:string;virtual;
  {$ENDIF MULTILIB}
  public
    function GetLibs(Basepath:string):boolean;override;
    {$ifndef FPCONLY}
    function GetLibsLCL(LCL_Platform:string; Basepath:string):boolean;override;
    {$endif}
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
    {$IFDEF MULTILIB}
    property MultilibDir:string read GetMultilibDir;
    property MultilibDirShort:string read GetMultilibDirShort;
    property ObjdumpOutput:string read GetObjdumpOutput;
    property LDOutput:string read GetLDOutput;
    {$ENDIF MULTILIB}
  end;

implementation

uses
  Process, FileUtil, fpcuputil;

{ Tany_linux }

{$IFDEF MULTILIB}

function Tany_linux.GetMultilibDir:string;
begin
  case TargetCPU of
    TCPU.i386: result:='i386-linux-gnu';
    TCPU.x86_64: result:='x86_64-linux-gnu';
  else
    result:='';
  end;
end;
function Tany_linux.GetMultilibDirShort:string;
begin
  case TargetCPU of
    TCPU.i386: result:='lib32';
    TCPU.x86_64: result:='lib64';
  else
    result:='';
  end;
end;
function Tany_linux.GetObjdumpOutput:string;
begin
  case TargetCPU of
    TCPU.i386: result:='elf32-i386';
    TCPU.x86_64: result:='elf64-x86-64';
  else
    result:='';
  end;
end;
function Tany_linux.GetLDOutput:string;
begin
  case TargetCPU of
    TCPU.i386: result:='elf_i386';
    TCPU.x86_64: result:='elf_x86_64';
  else
    result:='';
  end;
end;

function Tany_linux.CheckMultilib:boolean;
var
  s,magic:string;
begin
  result:=FMultilib;
  if result then exit;

  // Check if we have the multilib binary tools
  magic:=GetObjdumpOutput;
  if (Length(magic)=0) then exit;
  RunCommand('objdump',['-i'], s,[poUsePipes, poStderrToOutPut],swoHide);
  if AnsiPos(magic, s) <> 0 then
  begin
    magic:=GetLDOutput;
    if (Length(magic)=0) then exit;
    RunCommand('ld',['-V'], s,[poUsePipes, poStderrToOutPut],swoHide);
    if AnsiPos(magic, s) <> 0 then
    begin
      // Check if we have the libs
      magic:=GetMultilibDir;
      if (Length(magic)=0) then exit;
      s:='/lib/'+magic; //debian (multilib) Jessie+ convention
      if DirectoryExists(s) AND FileExists(s+DirectorySeparator+'libc.so.6') then
      begin
        s:='/usr/lib/'+magic; //debian (multilib) Jessie+ convention
        if DirectoryExists(s) AND FileExists(s+DirectorySeparator+'libX11.so.6')  AND FileExists(s+DirectorySeparator+'libgdk-x11-2.0.so') then FMultilib:=True;
      end;
    end;
  end;
  result:=FMultilib;
end;

{$ENDIF MULTILIB}

function Tany_linux.GetLibs(Basepath:string): boolean;
var
  aDirName,aLibName,s:string;
begin
  result:=FLibsFound;
  if result then exit;

  if FMUSL then
  begin
    aDirName:=TargetCPUName+'-musl'+TargetOSName;
    aLibName:='libc.musl-'+TargetCPUName+'.so.1';
  end
  else
  begin
    aDirName:=DirName;
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
      aLibName:='ld-musl-'+TargetCPUName+'.so.1';
      AddFPCCFGSnippet('-FL/lib/'+aLibName);
    end;
  end;

  if not result then
  begin
    {$IFDEF MULTILIB}
    if CheckMultilib then
    begin
      result:=true;
      FLibsFound:=True;

      FLibsPath:='/lib/'+GetMultilibDir;
      AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath));

      FLibsPath:='/usr/lib/'+GetMultilibDir;
      AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath));

      s:='/'+GetMultilibDirShort;
      if DirectoryExists(s) then
      begin
        s:=s+DirectorySeparator;
        AddFPCCFGSnippet('-Fl'+s);
      end;

      s:='/usr/'+GetMultilibDirShort;
      if DirectoryExists(s) then
      begin
        s:=s+DirectorySeparator;
        AddFPCCFGSnippet('-Fl'+s);
      end;

      // gcc multilib
      s:='';
      {$IFDEF CPUX64}
      s:=IncludeTrailingPathDelimiter(GetStartupObjects)+'32';
      {$ENDIF CPUX64}
      {$IFDEF CPUX86}
      s:=IncludeTrailingPathDelimiter(GetStartupObjects)+'64';
      {$ENDIF CPUX86}
      if DirectoryExists(s) then
      begin
        s:=s+DirectorySeparator;
        AddFPCCFGSnippet('-Fl'+s);
      end;

    end;
    {$ENDIF MULTILIB}
  end;

  SearchLibraryInfo(result);
end;

{$ifndef FPCONLY}
function Tany_linux.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least
  result:=inherited;
end;
{$endif}

function Tany_linux.GetBinUtils(Basepath:string): boolean;
var
  AsFile: string;
  BinPrefixTry: string;
  aDirName: string;
  s:string;
begin
  result:=inherited;

  if result then exit;

  if FMUSL then
    aDirName:=TargetCPUName+'-musl'+TargetOSName
  else
    aDirName:=DirName;

  AsFile:=FBinUtilsPrefix+'as'+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);

  {$IFDEF MULTILIB}
  if CheckMultilib then
  begin
    s:=Which('objdump');
    s:=ExtractFileDir(s);
    AsFile:='as'+GetExeExt;
    result:=SearchBinUtil(s,AsFile);
    if result then FBinUtilsPrefix:='';
  end;
  {$ENDIF}

  if not result then
    result:=SimpleSearchBinUtil(BasePath,aDirName,AsFile);

  // Now also allow for cpu-linux-gnu- binutilsprefix (e.g. codesourcery)
  if not result then
  begin
    BinPrefixTry:=Self.TargetCPUName+'-linux-gnu-';
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,aDirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  // Also allow for (cross)binutils without prefix in the right directory
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

constructor Tany_linux.Create;
begin
  inherited Create;
  FTargetOS:=TOS.linux;
  FAlreadyWarned:=false;
  {$IFDEF MULTILIB}
  FMultilib:=false;
  {$ENDIF MULTILIB}
end;

destructor Tany_linux.Destroy;
begin
  inherited Destroy;
end;

end.

