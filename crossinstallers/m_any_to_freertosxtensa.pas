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
  {$ifdef Unix}
  BaseUnix,
  {$endif}
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
  StaticLibName1='libesp32.a';
  StaticLibName2='libfreertos.a';
  StaticLibName3='libc.a';
var
  PresetLibPath:string;
  S:string;
begin
  result:=FLibsFound;
  if result then exit;

  if length(FSubArch)>0
     then ShowInfo('Cross-libs: We have a subarch: '+FSubArch)
     else ShowInfo('Cross-libs: No subarch defined. Expect fatal errors.',etError);

  // simple: check presence of library file in basedir
  result:=SearchLibrary(Basepath,LIBCNAME);
  // search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,LIBCNAME);

  // do the same as above, but look for a static esp lib
  if not result then
    result:=SearchLibrary(Basepath,StaticLibName1);
  // search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,StaticLibName1);

  // do the same as above, but look for a static freertos lib
  if not result then
    result:=SearchLibrary(Basepath,StaticLibName2);
  // search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,StaticLibName2);

  SearchLibraryInfo(result);

  if result then
  begin
    FLibsFound:=True;
    AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(FLibsPath));
  end;

  if (true) then
  begin
    PresetLibPath:=GetUserDir;
    {$IFDEF UNIX}
    //if FpGeteuid=0 then PresetLibPath:='/usr/local/lib';
    {$ENDIF}
    PresetLibPath:=ConcatPaths([PresetLibPath,'.espressif','tools','xtensa-esp32-elf']);
    S:=FindFileInDir(StaticLibName3,PresetLibPath);
    if (Length(S)>0) then
    begin
      PresetLibPath:=ExtractFilePath(S);
      AddFPCCFGSnippet('-Fl'+IncludeTrailingPathDelimiter(PresetLibPath));
    end;
  end;

end;

function TAny_FreeRTOSXtensa.GetBinUtils(Basepath:string): boolean;
var
  AsFile,aOption: string;
  S,PresetBinPath:string;
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

  if (not result) then
  begin
    PresetBinPath:=GetUserDir;
    {$IFDEF LINUX}
    if FpGetEUid=0 then PresetBinPath:='/usr/local/bin';
    {$ENDIF}
    PresetBinPath:=ConcatPaths([PresetBinPath,'.espressif','tools','xtensa-esp32-elf']);
    S:=FindFileInDir(AsFile,PresetBinPath);
    if (Length(S)>0) then
    begin
      PresetBinPath:=ExtractFilePath(S);
      result:=SearchBinUtil(PresetBinPath,AsFile);
    end;
  end;

  if (not result) then
  begin
    PresetBinPath:=Trim(GetEnvironmentVariable('IDF_TOOLS_PATH'));
    if (Length(PresetBinPath)>0) then
    begin
      S:=FindFileInDir(AsFile,PresetBinPath);
      if (Length(S)>0) then
      begin
        PresetBinPath:=ExtractFilePath(S);
        result:=SearchBinUtil(PresetBinPath,AsFile);
      end;
    end;
  end;

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
      aOption:='-Cplx6 ';
      FCrossOpts.Add(aOption+' ');
      ShowInfo('Did not find any -Cp architecture parameter; using '+aOption+' and SUBARCH='+FSubArch+'.');
    end else aOption:=Trim(FCrossOpts[i]);
    //AddFPCCFGSnippet(aOption);

    i:=StringListStartsWith(FCrossOpts,'-Cf');
    if i=-1 then
    begin
      aOption:='-Cfhard ';
      FCrossOpts.Add(aOption+' ');
      ShowInfo('Did not find any -Cf parameter; using '+aOption+'.');
    end else aOption:=Trim(FCrossOpts[i]);
    //AddFPCCFGSnippet(aOption);

    AddFPCCFGSnippet('-Wpesp32');

    S:=Trim(GetEnvironmentVariable('IDF_PATH'));
    if (Length(S)=0) then
    begin
      //AsFile:=ConcatPaths(['esp-idf','components','esptool_py','esptool','esptool.py']);
      PresetBinPath:=FBinUtilsPath;
      S:=FindFileInDir('esptool.py',PresetBinPath);
      if (Length(S)=0) then
      begin
        // go one directory up
        PresetBinPath:=ExpandFileName(IncludeTrailingPathDelimiter(FBinUtilsPath)+'..');
        S:=FindFileInDir('esptool.py',PresetBinPath);
      end;
      if (Length(S)>0) then
      begin
        repeat
          S:=ExtractFileDir(S);
          AsFile:=ExtractFileName(S);
        until ((AsFile='components') OR (Length(AsFile)=0));
        S:=ExtractFileDir(S);
        if (Length(S)>0) then
          AddFPCCFGSnippet('-Ff'+S); {Set the IDF SDK path};
      end;
    end;

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

