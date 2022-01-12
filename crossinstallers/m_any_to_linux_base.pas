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

      if (NOT FMultilib) then
      begin
        //debian (multilib) Jessie+ convention
        magic:=GetMultilibDir;
        if (Length(magic)=0) then exit;
        s:='/lib/'+magic;
        if DirectoryExists(s) AND FileExists(s+DirectorySeparator+'libc.so.6') then
        begin
          s:='/usr/lib/'+magic; //debian (multilib) Jessie+ convention
          if DirectoryExists(s) AND FileExists(s+DirectorySeparator+'libX11.so.6')  AND FileExists(s+DirectorySeparator+'libgdk-x11-2.0.so') then FMultilib:=True;
        end;
      end;

      if (NOT FMultilib) then
      begin
        //Arch Linux convention
        magic:=GetMultilibDirShort;
        if (Length(magic)=0) then exit;
        s:='/usr/'+magic;
        if DirectoryExists(s) AND FileExists(s+DirectorySeparator+'libc.so.6') then
        begin
          if DirectoryExists(s) AND FileExists(s+DirectorySeparator+'libX11.so.6')  AND FileExists(s+DirectorySeparator+'libgdk-x11-2.0.so') then FMultilib:=True;
        end;
      end;


    end;
  end;
  result:=FMultilib;
end;

{$ENDIF MULTILIB}

function Tany_linux.GetLibs(Basepath:string): boolean;
const
  SDSTARTMAGIC='SEARCH_DIR("=';
  SDENDMAGIC='");';
var
  aDirName,aLibName,s,sd:string;
  i,j:integer;
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
    AddFPCCFGSnippet('-Xd'); // do not pass parent /lib etc dir to linker}
    AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath)); // the directory to look for the target  libraries}

    //if CheckMultilib then
    if false then
    begin
      {$IFDEF CPU64}
      if TargetCPU in CPUADDRSIZE_32 then
        AddFPCCFGSnippet('-Xr/usr/lib32');
      {$ENDIF CPU64}
      {$IFDEF CPU32}
      if TargetCPU in CPUADDRSIZE_64 then
        AddFPCCFGSnippet('-Xr/usr/lib64');
      {$ENDIF CPU32}
    end;

    if FMUSL then
    begin
      aLibName:='ld-musl-'+TargetCPUName+'.so.1';
      AddFPCCFGSnippet('-FL/lib/'+aLibName);
    end;
  end;

  if (NOT result) then
  begin
    {$IFDEF MULTILIB}
    if CheckMultilib then
    begin
      result:=true;
      FLibsFound:=True;

      // We should use -XR to define (or limit) the linker search path.
      // But fpcupdeluxe only will add all suitable library paths
      // So we need to disable the default linker search path to prevent linking errors.
      AddFPCCFGSnippet('-Xd'); // do not pass parent /lib etc dir to linker}

      s:='/lib/'+GetMultilibDir;
      if DirectoryExists(s) then
      begin
        FLibsPath:=s;
        s:=s+DirectorySeparator;
        AddFPCCFGSnippet('-Fl'+s);
      end;

      s:='/usr/lib/'+GetMultilibDir;
      if DirectoryExists(s) then
      begin
        FLibsPath:=s;
        s:=s+DirectorySeparator;
        AddFPCCFGSnippet('-Fl'+s);
      end;

      s:='/'+GetMultilibDirShort;
      if DirectoryExists(s) then
      begin
        FLibsPath:=s;
        s:=s+DirectorySeparator;
        AddFPCCFGSnippet('-Fl'+s);
      end;

      s:='/usr/'+GetMultilibDirShort;
      if DirectoryExists(s) then
      begin
        FLibsPath:=s;
        s:=s+DirectorySeparator;
        AddFPCCFGSnippet('-Fl'+s);
      end;
    end;
    {$ENDIF MULTILIB}

    {$IFDEF MULTILIB}
    if (NOT CheckMultilib) then
    {$ELSE}
    if true then
    {$ENDIF}
    begin
      {$IFDEF UNIX}
      if FBinsFound then
      begin
        s:='';
        RunCommand(IncludeTrailingPathDelimiter(FBinUtilsPath)+FBinUtilsPrefix+'ld',['--verbose'], s,[poUsePipes, poStderrToOutPut],swoHide);
        repeat
          i:=Pos(SDSTARTMAGIC,s);
          if (i>0) then
          begin
            Delete(s,1,(i-1)+Length(SDSTARTMAGIC));
            j:=Pos(SDENDMAGIC,s);
            if (j>0) then
            begin
              sd:=Copy(s,1,(j-1));
              writeln(sd);
              if DirectoryExists(sd) then
              begin
                if (NOT result) then
                begin
                  result:=SearchLibrary(sd,aLibName);
                  if result then
                  begin
                    FLibsFound:=True;
                    AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath)); // the directory to look for the target  libraries}
                  end;
                end;
              end;
              Delete(s,1,(j-1)+Length(SDENDMAGIC));
            end
            else i:=0;
          end;
        until (i<1);
      end;
      {$ENDIF UNIX}
    end;
  end;

  if result then
  begin
    // Add gcc path if any
    {$IFDEF UNIX}
    {$ifdef CPU64}
    if TargetCPU in CPUADDRSIZE_32 then
    begin
      s:=IncludeTrailingPathDelimiter(GetStartupObjects)+'32';
      if DirectoryExists(s) then
        AddFPCCFGSnippet('-Fl'+s+DirectorySeparator);
    end;
    {$endif CPU64}
    {$ifdef CPU32}
    if TargetCPU in CPUADDRSIZE_64 then
    begin
      s:=IncludeTrailingPathDelimiter(GetStartupObjects)+'64';
      if DirectoryExists(s) then
        AddFPCCFGSnippet('-Fl'+s+DirectorySeparator);
    end;
    {$endif CPU32}
    {$ENDIF UNIX}
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
  s: string;
  i: integer;
begin
  result:=inherited;

  if result then exit;

  if FMUSL then
    aDirName:=TargetCPUName+'-musl'+TargetOSName
  else
    aDirName:=DirName;

  BinPrefixTry:=FBinUtilsPrefix;

  AsFile:=BinPrefixTry+'as'+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,aDirName,AsFile);

  {$IFDEF UNIX}
  // User may also have placed them into their regular search path:
  if (not result) then
  begin
    for i:=Low(UnixBinDirs) to High(UnixBinDirs) do
    begin
      result:=SearchBinUtil(IncludeTrailingPathDelimiter(UnixBinDirs[i])+aDirName, AsFile);
      if (not result) then
        result:=SearchBinUtil(UnixBinDirs[i], AsFile);
      if result then break;
    end;
  end;
  {$ENDIF UNIX}

  {$IFDEF UNIX}
  {$IFDEF MULTILIB}
  if (not result) then
  begin
    if CheckMultilib then
    begin
      BinPrefixTry:='';
      s:=Which('objdump');
      s:=ExtractFileDir(s);
      AsFile:='as'+GetExeExt;
      result:=SearchBinUtil(s,AsFile);
      if not result then
        result:=SimpleSearchBinUtil(BasePath,aDirName,AsFile);
    end;
  end;
  {$ENDIF}
  {$ENDIF UNIX}

  // Now also allow for cpu-linux-gnu- binutilsprefix (e.g. codesourcery)
  if not result then
  begin
    BinPrefixTry:=TargetCPUName+'-linux-gnu-';
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if (not result) then
      result:=SimpleSearchBinUtil(BasePath,aDirName,AsFile);
    {$IFDEF UNIX}
    if (not result) then
    begin
      for i:=Low(UnixBinDirs) to High(UnixBinDirs) do
      begin
        result:=SearchBinUtil(UnixBinDirs[i], AsFile);
        if result then break;
      end;
    end;
    {$ENDIF UNIX}
    if (not result) then
    begin
      if (TargetCPU=TCPU.i386) then
      begin
        BinPrefixTry:='i586-linux-gnu-';
        AsFile:=BinPrefixTry+'as'+GetExeExt;
        result:=SearchBinUtil(BasePath,AsFile);
        if (not result) then
          result:=SimpleSearchBinUtil(BasePath,aDirName,AsFile);
        {$IFDEF UNIX}
        if (not result) then
        begin
          for i:=Low(UnixBinDirs) to High(UnixBinDirs) do
          begin
            result:=SearchBinUtil(UnixBinDirs[i], AsFile);
            if result then break;
          end;
        end;
        {$ENDIF UNIX}
      end;
    end;
    if (not result) then
    begin
      if (TargetCPU=TCPU.i386) then
      begin
        BinPrefixTry:='i686-linux-gnu-';
        AsFile:=BinPrefixTry+'as'+GetExeExt;
        result:=SearchBinUtil(BasePath,AsFile);
        if (not result) then
          result:=SimpleSearchBinUtil(BasePath,aDirName,AsFile);
        {$IFDEF UNIX}
        if (not result) then
        begin
          for i:=Low(UnixBinDirs) to High(UnixBinDirs) do
          begin
            result:=SearchBinUtil(UnixBinDirs[i], AsFile);
            if result then break;
          end;
        end;
        {$ENDIF UNIX}
      end;
    end;
  end;

  {$IFDEF LINUX}
  // Also allow for correctly named suse (cross)binutils in /usr/bin
  if (not result) then
  begin
    BinPrefixTry:=TargetCPUName+'-suse-linux-';
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if (not result) then
      result:=SimpleSearchBinUtil(BasePath,aDirName,AsFile);
    if (not result) then
    begin
      for i:=Low(UnixBinDirs) to High(UnixBinDirs) do
      begin
        result:=SearchBinUtil(UnixBinDirs[i], AsFile);
        if result then break;
      end;
    end;
    if (not result) then
    begin
      if (TargetCPU=TCPU.i386) then
      begin
        BinPrefixTry:='i586-suse-linux-';
        AsFile:=BinPrefixTry+'as'+GetExeExt;
        result:=SearchBinUtil(BasePath,AsFile);
        if (not result) then
          result:=SimpleSearchBinUtil(BasePath,aDirName,AsFile);
        if (not result) then
        begin
          for i:=Low(UnixBinDirs) to High(UnixBinDirs) do
          begin
            result:=SearchBinUtil(UnixBinDirs[i], AsFile);
            if result then break;
          end;
        end;
      end;
    end;
    if (not result) then
    begin
      if (TargetCPU=TCPU.i386) then
      begin
        BinPrefixTry:='i686-suse-linux-';
        AsFile:=BinPrefixTry+'as'+GetExeExt;
        result:=SearchBinUtil(BasePath,AsFile);
        if (not result) then
          result:=SimpleSearchBinUtil(BasePath,aDirName,AsFile);
        if (not result) then
        begin
          for i:=Low(UnixBinDirs) to High(UnixBinDirs) do
          begin
            result:=SearchBinUtil(UnixBinDirs[i], AsFile);
            if result then break;
          end;
        end;
      end;
    end;
  end;
  {$ENDIF LINUX}

  // Also allow for (cross)binutils without prefix in the right directory
  if (not result) then
  begin
    BinPrefixTry:='';
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if (not result) then
      result:=SimpleSearchBinUtil(BasePath,aDirName,AsFile);
  end;

  if result then FBinUtilsPrefix:=BinPrefixTry;

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

