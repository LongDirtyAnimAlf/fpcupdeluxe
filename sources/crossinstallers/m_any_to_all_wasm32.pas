unit m_any_to_all_wasm32;
{ Cross compiles from any platform with correct binutils to WebAssembly
Copyright (C) 2021 Alf

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
  Classes, SysUtils, m_crossinstaller;

type
  TAny_AllWasm32 = class(TCrossInstaller)
  private
    FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
  public
    function GetLibs(Basepath:string):boolean;override;
    function GetBinUtils(Basepath:string):boolean;override;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  fpcuputil;

{ TAny_AllWasm32 }

function TAny_AllWasm32.GetLibs(Basepath:string): boolean;
const
  LibName='libc.a';
begin
  result:=inherited;
  if result then exit;

  result:=SearchLibrary(Basepath,LibName);
  if not result then
     result:=SimpleSearchLibrary(BasePath,DirName,LibName);

  if not result then
    result:=SimpleSearchLibrary(BasePath,TargetCPUName+'-all',LibName);

  SearchLibraryInfo(result);

  if result then
  begin
    FLibsFound:=True;
    AddFPCCFGSnippet('-Fl'+LibsPath);
  end
  else
  begin
    if TargetOS=TOS.embedded then
    begin
      // Do not fail
      FLibsPath:='';
      FLibsFound:=True;
      result:=true;
    end;
  end;

end;

function TAny_AllWasm32.GetBinUtils(Basepath:string): boolean;
// webassembly uses internal assembler.
// so, look for linker
const
  LDNAME = 'wasm-ld';
  MAGIC = 'WASMOSTARGETMAGIC';
  LDNAMES :array[0..1] of string = (LDNAME,'wasm32-'+MAGIC+'-'+LDNAME);
var
  s,LdFile: string;
begin
  result:=inherited;
  if result then exit;

  FBinUtilsPrefix:='';

  for s in LDNAMES do
  begin
    LdFile:=s;
    if Pos(MAGIC,s)>0 then
      LdFile:=StringReplace(s,MAGIC,TargetOSName,[])
    else
      LdFile:=s;

    LdFile:=LdFile+GetExeExt;

    result:=SearchBinUtil(BasePath,LdFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,DirName,LdFile);
    if not result then
      result:=SimpleSearchBinUtil(BasePath,TargetCPUName+'-all',LdFile);

    if result then break;
  end;

  if (not result) then
  begin
    {$ifdef UNIX}
    // Look in PATH for suitable binaries
    if (not result) then
    begin
      LdFile:=Which(LDNAME);
      if ((Length(LdFile)>0) AND FileExists(LdFile)) then
      begin
        if LdFile=LDNAME then LdFile:=ExpandFileName(LdFile);
        FBinUtilsPath:=ExtractFilePath(LdFile);
        result:=true;
      end;
    end;
    if (not result) then
    begin
      s:=TargetCPUName+'-'+TargetOSName+'-';
      LdFile:=Which(s+LDNAME);
      if ((Length(LdFile)>0) AND FileExists(LdFile)) then
      begin
        FBinUtilsPath:=ExtractFilePath(LdFile);
        FBinUtilsPrefix:=s;
        result:=true;
      end;
    end;
    if (not result) then
    begin
      s:='wasm32-'+TargetOSName+'-';
      LdFile:=Which(s+LDNAME);
      if ((Length(LdFile)>0) AND FileExists(LdFile)) then
      begin
        FBinUtilsPath:=ExtractFilePath(LdFile);
        FBinUtilsPrefix:=s;
        result:=true;
      end;
    end;

    {$ifdef DARWIN}
    // Look for brew installs
    if (not result) then
    begin
      LdFile:='/usr/local/opt/llvm/bin/'+LDNAME;
      if (FileExists(LdFile)) then
      begin
        FBinUtilsPath:=ExtractFilePath(LdFile);
        result:=true;
      end;
    end;
    {$endif DARWIN}
    {$endif UNIX}
  end;

  SearchBinUtilsInfo(result);

  if result then
  begin
    FBinsFound:=true;
    // Configuration snippet for FPC
    AddFPCCFGSnippet('-FD'+BinUtilsPath);
    AddFPCCFGSnippet('-XP'+BinUtilsPrefix); {Prepend the binutils names};
    {$ifdef UNIX}
    //AddFPCCFGSnippet('-Cg-'; {Disable PIC code};
    {$endif UNIX}
  end;
end;

constructor TAny_AllWasm32.Create;
begin
  inherited Create;
  FBinutilsPathInPath:=true;
  FAlreadyWarned:=false;
end;

destructor TAny_AllWasm32.Destroy;
begin
  inherited Destroy;
end;

end.

