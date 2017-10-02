unit m_any_to_embeddedmipsel;
{ Cross compiles from any platform with correct binutils to Embedded Mipsel
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

{
Setup: based on cross binaries from
http://svn.freepascal.org/svn/fpcbuild/binaries/i386-win32/
with binutils 2.22

Add a cross directory under the fpcup "root" installdir directory (e.g. c:\development\cross, and e.g. regular fpc sources in c:\development\fpc)
Then place the binaries in c:\development\cross\bin\mipsel-embedded
Binaries include
mipsel-embedded-ar.exe
mipsel-embedded-as.exe
mipsel-embedded-ld.exe
mipsel-embedded-objcopy.exe
mipsel-embedded-objdump.exe
mipsel-embedded-strip.exe
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, m_crossinstaller, fileutil, fpcuputil;

implementation
type

{ TAny_Embeddedmipsel }
TAny_Embeddedmipsel = class(TCrossInstaller)
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

{ TAny_Embeddedmipsel }

function TAny_Embeddedmipsel.GetLibs(Basepath:string): boolean;
const
  DirName='mipsel-embedded';
  LibName='';
begin
  // mipsel-embedded does not need libs by default, but user can add them.

  result:=FLibsFound;
  if result then exit;

  // search local paths based on libbraries provided for or adviced by fpc itself
  result:=SimpleSearchLibrary(BasePath,DirName,LibName);

  if result then
  begin
    //todo: check if -XR is needed for fpc root dir Prepend <x> to all linker search paths
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-Fl'+IncludeTrailingPathDelimiter(FLibsPath) {buildfaq 1.6.4/3.3.1:  the directory to look for the target  libraries};
    SearchLibraryInfo(result);
  end
  else
  begin
    //libs path is optional; it can be empty
    ShowInfo('Libspath ignored; it is optional for this cross compiler.');
    FLibsPath:='';
    result:=true;
  end;
  FLibsFound:=True;
end;

{$ifndef FPCONLY}
function TAny_Embeddedmipsel.GetLibsLCL(LCL_Platform: string; Basepath: string): boolean;
begin
  // todo: get gtk at least, add to FFPCCFGSnippet
  ShowInfo('Todo: implement lcl libs path from basepath '+BasePath,etdebug);
  result:=inherited;
end;
{$endif}

function TAny_Embeddedmipsel.GetBinUtils(Basepath:string): boolean;
const
  DirName='mipsel-embedded';
var
  AsFile: string;
  BinPrefixTry: string;
begin
  result:=inherited;
  if result then exit;

  // Start with any names user may have given
  AsFile:=FBinUtilsPrefix+'as'+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  // Now also allow for mipsel- binutilsprefix
  if not result then
  begin
    BinPrefixTry:='mipsel-';
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  // Now also allow for pic32- binutilsprefix
  if not result then
  begin
    BinPrefixTry:='pic32-';
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  // Now also allow for xc32- binutilsprefix
  if not result then
  begin
    BinPrefixTry:='xc32-';
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  // Now also allow for empty binutilsprefix:
  if not result then
  begin
    BinPrefixTry:='';
    AsFile:=BinPrefixTry+'as'+GetExeExt;
    result:=SearchBinUtil(BasePath,AsFile);
    if not result then result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);
    if result then FBinUtilsPrefix:=BinPrefixTry;
  end;

  SearchBinUtilsInfo(result);

  if not result then
  begin
    {$ifdef mswindows}
    ShowInfo('Suggestion for pic32 cross binutils: http://chipkit.s3.amazonaws.com');
    {$endif}
    FAlreadyWarned:=true;
  end
  else
  begin
    FBinsFound:=true;

    (*
    if StringListStartsWith(FCrossOpts,'-Cp')=-1 then
    begin
      FCrossOpts.Add('-Cppic32'); // Teensy default
      ShowInfo('Did not find any -Cp architecture parameter; using -Cppic32.');
    end;
    *)

    // Configuration snippet for FPC
    FFPCCFGSnippet:=FFPCCFGSnippet+LineEnding+
    '-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath)+LineEnding+ {search this directory for compiler utilities}
    '-XP'+FBinUtilsPrefix; {Prepend the binutils names}
  end;
end;

constructor TAny_Embeddedmipsel.Create;
begin
  inherited Create;
  FBinUtilsPrefix:='mipsel-embedded-';
  FBinUtilsPath:='';
  FFPCCFGSnippet:=''; //will be filled in later
  //FCompilerUsed:=ctInstalled;
  FLibsPath:='';
  FTargetCPU:='mipsel';
  FTargetOS:='embedded';
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor TAny_Embeddedmipsel.Destroy;
begin
  inherited Destroy;
end;

var
  Any_Embeddedmipsel:TAny_Embeddedmipsel;

initialization
  Any_Embeddedmipsel:=TAny_Embeddedmipsel.Create;
  RegisterExtension(Any_Embeddedmipsel.TargetCPU+'-'+Any_Embeddedmipsel.TargetOS,Any_Embeddedmipsel);
finalization
  Any_Embeddedmipsel.Destroy;
end.

