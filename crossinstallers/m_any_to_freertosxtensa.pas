unit m_any_to_freertosxtensa;
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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  FileUtil, m_crossinstaller, fpcuputil;

type

{ TAny_FreeRTOSXtensa }
TAny_FreeRTOSXtensa = class(TCrossInstaller)
private
  FAlreadyWarned: boolean; //did we warn user about errors and fixes already?
public
  function GetLibs(Basepath:string):boolean;override;
  function GetBinUtils(Basepath:string):boolean;override;
  constructor Create;
  destructor Destroy; override;
end;

{ TAny_FreeRTOSXtensa }

function TAny_FreeRTOSXtensa.GetLibs(Basepath:string): boolean;
const
  StaticLibName='libc.a';
begin
  result:=FLibsFound;
  if result then exit;

  if length(FSubArch)>0
     then ShowInfo('Cross-libs: We have a subarch: '+FSubArch)
     else ShowInfo('Cross-libs: No subarch defined. Expect fatal errors.',etError);

  // begin simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LIBCNAME);
  // search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,LIBCNAME);

  // do the same as above, but look for a static lib
  result:=SearchLibrary(Basepath,StaticLibName);
  // search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,StaticLibName);

  SearchLibraryInfo(result);

  if result then
  begin
    FLibsFound:=True;
    AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath));
  end;
end;

function TAny_FreeRTOSXtensa.GetBinUtils(Basepath:string): boolean;
var
  AsFile,aOption: string;
  i:integer;
begin
  result:=inherited;
  if result then exit;

  FBinUtilsPrefix:=GetCPU(TargetCPU)+'-esp32-elf-';

  // Start with any names user may have given
  AsFile:=FBinUtilsPrefix+'as'+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  SearchBinUtilsInfo(result);

  if result then
  begin
    FBinsFound:=true;

    // Configuration snippet for FPC
    AddFPCCFGSnippet('-FD'+IncludeTrailingPathDelimiter(FBinUtilsPath));
    AddFPCCFGSnippet('-XP'+FBinUtilsPrefix); {Prepend the binutils names};

    i:=StringListStartsWith(FCrossOpts,'-Cp');
    if i=-1 then
    begin
      if length(FSubArch)=0 then FSubArch:='lx6';
      aOption:='-Cplx6 -Cfhard';
      FCrossOpts.Add(aOption+' ');
      ShowInfo('Did not find any -Cp architecture parameter; using '+aOption+' and SUBARCH='+FSubArch+'.');
    end else aOption:=Trim(FCrossOpts[i]);
    AddFPCCFGSnippet(aOption);

  end;
end;

constructor TAny_FreeRTOSXtensa.Create;
begin
  inherited Create;
  FTargetCPU:=TCPU.xtensa;
  FTargetOS:=TOS.freertos;
  Reset;
  FAlreadyWarned:=false;
  ShowInfo;
end;

destructor TAny_FreeRTOSXtensa.Destroy;
begin
  inherited Destroy;
end;

var
  Any_FreeRTOSXtensa:TAny_FreeRTOSXtensa;

initialization
  Any_FreeRTOSXtensa:=TAny_FreeRTOSXtensa.Create;
  RegisterCrossCompiler(Any_FreeRTOSXtensa.RegisterName,Any_FreeRTOSXtensa);

finalization
  Any_FreeRTOSXtensa.Destroy;
end.

