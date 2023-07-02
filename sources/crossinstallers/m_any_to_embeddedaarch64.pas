unit m_any_to_embeddedaarch64;
{ Cross compiles from any platform with correct binutils to Embedded ARM
Copyright (C) 2017 Alf

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

{ TAny_Embeddedaarch64 }
TAny_Embeddedaarch64 = class(TCrossInstaller)
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

{ TAny_Embeddedaarch64 }

function TAny_Embeddedaarch64.GetLibs(Basepath:string): boolean;
const
  LibName='libgcc.a';  // is this correct ??
var
  S:string;
begin
  // Arm-embedded does not need libs by default, but user can add them.
  result:=inherited;

  if result then exit;

  if (FSubArch<>TSUBARCH.saNone) then
  begin
    ShowInfo('Cross-libs: We have a subarch: '+SubArchName);
  end
  else ShowInfo('Cross-libs: No subarch defined. Expect fatal errors.',etError);

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LibName);
  // search local paths based on libraries provided for or adviced by fpc itself
  if not result then
     if (FSubArch<>TSUBARCH.saNone) then result:=SimpleSearchLibrary(BasePath,IncludeTrailingPathDelimiter(DirName)+SubArchName,LibName);
  if not result then
     result:=SimpleSearchLibrary(BasePath,DirName,LibName);

  SearchLibraryInfo(result);

  if result then
  begin
    FLibsFound:=True;

    if PerformLibraryPathMagic(S) then
    begin
      AddFPCCFGSnippet('-Fl'+S,false);
    end
    else
    begin
      // If we do not have magic, add subarch to enclose
      AddFPCCFGSnippet('#IFDEF CPU'+UpperCase(SubArchName));
      AddFPCCFGSnippet('-Fl'+S);
      AddFPCCFGSnippet('#ENDIF CPU'+UpperCase(SubArchName));
    end;
  end;

  if not result then
  begin
    //libs path is optional; it can be empty
    ShowInfo('Libspath ignored; it is optional for this cross compiler.');
    FLibsPath:='';
    FLibsFound:=True;
    result:=true;
  end;
end;

{$ifndef FPCONLY}
function TAny_Embeddedaarch64.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least, add to FFPCCFGSnippet
  ShowInfo('Todo: implement lcl libs path from basepath '+BasePath,etdebug);
  result:=inherited;
end;
{$endif}

function TAny_Embeddedaarch64.GetBinUtils(Basepath:string): boolean;
var
  AsFile: string;
  BinPrefixTry: string;
  {$ifdef unix}
  i:integer;
  {$endif unix}
begin
  result:=inherited;
  if result then exit;

  // Start with any names user may have given
  AsFile:=BinUtilsPrefix+ASFILENAME+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  {$ifdef unix}
  // User may also have placed them into their regular search path:
  if not result then
  begin
    for i:=Low(UnixBinDirs) to High(UnixBinDirs) do
    begin
      result:=SearchBinUtil(IncludeTrailingPathDelimiter(UnixBinDirs[i])+DirName, AsFile);
      if not result then result:=SearchBinUtil(UnixBinDirs[i], AsFile);
      if result then break;
    end;
  end;
  {$endif unix}

  // Now also allow for arm-none-eabi- binutilsprefix (e.g. launchpadlibrarian)
  if not result then
  begin
    BinPrefixTry:='aarch64-elf-';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  // Now also allow for empty binutilsprefix in the right directory:
  if not result then
  begin
    BinPrefixTry:='';
    AsFile:=BinPrefixTry+ASFILENAME+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  SearchBinUtilsInfo(result);

  if not result then
  begin
    FAlreadyWarned:=true;
  end
  else
  begin
    FBinsFound:=true;
    // Configuration snippet for FPC
    AddFPCCFGSnippet('-FD'+BinUtilsPath);
    AddFPCCFGSnippet('-XP'+BinUtilsPrefix); {Prepend the binutils names};
  end;
end;

constructor TAny_Embeddedaarch64.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.aarch64;
  FTargetOS:=TOS.embedded;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor TAny_Embeddedaarch64.Destroy;
begin
  inherited Destroy;
end;

var
  Any_Embeddedaarch64:TAny_Embeddedaarch64;

initialization
  Any_Embeddedaarch64:=TAny_Embeddedaarch64.Create;
  RegisterCrossCompiler(Any_Embeddedaarch64.RegisterName,Any_Embeddedaarch64);

finalization
  Any_Embeddedaarch64.Destroy;
end.

