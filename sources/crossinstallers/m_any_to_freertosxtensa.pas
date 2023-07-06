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
  {$IFDEF LINUX}
  BaseUnix,
  {$ENDIF LINUX}
  FileUtil, StrUtils, m_crossinstaller, fpcuputil;

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
  StaticLibName1='libfreertos.a';
  StaticLibName2='libc.a';
var
  PresetLibPath:string;
  StaticLibNameESP:string;
  S:string;
  aABI:TABI;
  ActionNeeded:boolean;
begin
  result:=inherited;
  if result then exit;

  StaticLibNameESP:='';

  if (FSubArch<>TSUBARCH.saNone) then
  begin
    if (FSubArch=TSUBARCH.lx6) then StaticLibNameESP:='libesp32.a';
    if (FSubArch=TSUBARCH.lx106) then StaticLibNameESP:='libesp8266.a';
    ShowInfo('Cross-libs: We have a subarch: '+SubarchName);
  end
  else ShowInfo('Cross-libs: No subarch defined. Expect fatal errors.',etError);

  // simple: check presence of library file in basedir

  if (Length(StaticLibNameESP)>0) then
  begin
    if not result then
      result:=SearchLibrary(Basepath,StaticLibNameESP);
    // search local paths based on libbraries provided for or adviced by fpc itself
    if not result then
      result:=SimpleSearchLibrary(BasePath,DirName,StaticLibNameESP);
    if ((not result) AND (FSubArch<>TSUBARCH.saNone)) then
      result:=SimpleSearchLibrary(BasePath,IncludeTrailingPathDelimiter(DirName)+SubarchName,StaticLibNameESP);
  end;

  // do the same as above, but look for a static freertos lib
  if not result then
    result:=SearchLibrary(Basepath,StaticLibName1);
  // search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,StaticLibName1);
  if ((not result) AND (FSubArch<>TSUBARCH.saNone)) then
  begin
    result:=SimpleSearchLibrary(BasePath,ConcatPaths([DirName,SubarchName]),StaticLibName1);
    if (not result) then
      result:=SimpleSearchLibrary(BasePath,ConcatPaths([DirName,SubarchName,'debug']),StaticLibName1);
  end;

  // do the same as above, but look for a static libc_nano lib
  if not result then
    result:=SearchLibrary(Basepath,StaticLibName2);

  // search local paths based on libbraries provided for or adviced by fpc itself
  if not result then
    result:=SimpleSearchLibrary(BasePath,DirName,StaticLibName2);
  if ((not result) AND (FSubArch<>TSUBARCH.saNone)) then
  begin
    result:=SimpleSearchLibrary(BasePath,ConcatPaths([DirName,SubarchName]),StaticLibName2);
    if (not result) then
      result:=SimpleSearchLibrary(BasePath,ConcatPaths([DirName,SubarchName,'debug']),StaticLibName2);
  end;

  if ((not result) AND (FSubArch<>TSUBARCH.saNone)) then
  begin
    result:=SimpleSearchLibrary(BasePath,ConcatPaths([DirName,SubarchName]),StaticLibName2);
    if (not result) then
    begin
      for aABI in TABI do
      begin
        if aABI=TABI.default then continue;
        result:=SimpleSearchLibrary(BasePath,ConcatPaths([DirName,SubarchName,GetABI(aABI)]),StaticLibName2);
        if result then break;
      end;
    end;
  end;

  SearchLibraryInfo(result);

  if (result) then
  begin
    FLibsFound:=True;

    AddFPCCFGSnippet('-Xd'); {buildfaq 3.4.1 do not pass parent /lib etc dir to linker}

    ActionNeeded:=(NOT PerformLibraryPathMagic(S));

    if (NOT ActionNeeded) then AddFPCCFGSnippet('-Fl'+S,false);

    if (SubArch<>TSUBARCH.saNone) then
    begin
      AddFPCCFGSnippet('#IFDEF CPU'+UpperCase(SubArchName));

      if ActionNeeded then AddFPCCFGSnippet('-Fl'+S);

      // Add SDK libs path, if any
      PresetLibPath:=GetUserDir;
      {$IFDEF UNIX}
      //if FpGeteuid=0 then PresetLibPath:='/usr/local/lib';
      {$ENDIF}
      S:='';
      if (FSubArch=TSUBARCH.lx6) then
      begin
        PresetLibPath:=ConcatPaths([PresetLibPath,'.espressif','tools','xtensa-esp32-elf']);
        S:=FindFileInDir(StaticLibName1,PresetLibPath);
      end;
      if (FSubArch=TSUBARCH.lx106) then
      begin
        PresetLibPath:=ConcatPaths([PresetLibPath,'.espressif','tools','xtensa-lx106-elf']);
        S:=FindFileInDir(StaticLibName1,PresetLibPath);
      end;
      if (Length(S)>0) then
      begin
        S:='-Fl'+ExtractFileDir(S);
        AddFPCCFGSnippet(S);
      end;

      // Check tools deployment version
      // If not found, add default value
      ActionNeeded:=true;
      for S in FCrossOpts do
      begin
        if AnsiStartsStr('-WP',S) then
        begin
          ActionNeeded:=false;
          break;
        end;
      end;
      if (ActionNeeded) then
      begin
        S:='';
        if (FSubArch=TSUBARCH.lx6) then S:='4.3.2';
        if (FSubArch=TSUBARCH.lx106) then S:='3.4';
        if (Length(S)<>0) then
        begin
          S:='-WP'+S;
          AddFPCCFGSnippet(S);
          S:=ConcatPaths([BasePath,CROSSLIBPATH,TargetCPUName+'-'+TargetOSName]);
          PresetLibPath:=ConcatPaths([S,SubArchName]);
          if DirectoryExists(PresetLibPath) then AddFPCCFGSnippet('-Fl'+PresetLibPath);
        end;
      end;
      AddFPCCFGSnippet('#ENDIF CPU'+UpperCase(SubArchName));
    end;
  end;
end;

function TAny_FreeRTOSXtensa.GetBinUtils(Basepath:string): boolean;
var
  AsFile: string;
  S,FilePath,ToolVersion:string;
  ESPToolFiles: TStringList;
begin
  result:=inherited;
  if result then exit;

  if (FSubArch<>TSUBARCH.saNone) then
  begin
    S:=GetEnumNameSimple(TypeInfo(TSUBARCH),Ord(FSubArch));
    ShowInfo('Cross-libs: We have a subarch: '+S);
    if (FSubArch=TSUBARCH.lx6) then
      FBinUtilsPrefix:=TargetCPUName+'-esp32-elf-';
    if (FSubArch=TSUBARCH.lx106) then
      FBinUtilsPrefix:=TargetCPUName+'-lx106-elf-';
  end
  else ShowInfo('Cross-libs: No subarch defined. Expect fatal errors.',etError);

  // Start with any names user may have given
  AsFile:=BinUtilsPrefix+ASFILENAME+GetExeExt;

  result:=SearchBinUtil(BasePath,AsFile);
  if not result then
    result:=SimpleSearchBinUtil(BasePath,DirName,AsFile);

  if (not result) then
  begin
    FilePath:=GetUserDir;
    {$IFDEF LINUX}
    if FpGetEUid=0 then FilePath:='/usr/local/bin';
    {$ENDIF}
    if (not result) then
    begin
      if (FSubArch=TSUBARCH.lx6) then
      begin
        FilePath:=ConcatPaths([FilePath,'.espressif','tools','xtensa-esp32-elf']);
        S:=FindFileInDir(AsFile,FilePath);
        if (Length(S)>0) then
        begin
          FilePath:=ExtractFilePath(S);
          result:=SearchBinUtil(FilePath,AsFile);
        end;
      end;
    end;
    if (not result) then
    begin
      if (FSubArch=TSUBARCH.lx106) then
      begin
        FilePath:=ConcatPaths([FilePath,'.espressif','tools','xtensa-lx106-elf']);
        S:=FindFileInDir(AsFile,FilePath);
        if (Length(S)>0) then
        begin
          FilePath:=ExtractFilePath(S);
          result:=SearchBinUtil(FilePath,AsFile);
        end;
      end;
    end;
  end;

  if (not result) then
  begin
    FilePath:=Trim(GetEnvironmentVariable('IDF_TOOLS_PATH'));
    if (Length(FilePath)>0) then
    begin
      S:=FindFileInDir(AsFile,FilePath);
      if (Length(S)>0) then
      begin
        FilePath:=ExtractFilePath(S);
        result:=SearchBinUtil(FilePath,AsFile);
      end;
    end;
  end;

  SearchBinUtilsInfo(result);

  if result then
  begin
    FBinsFound:=true;

    if (FSubArch<>TSUBARCH.saNone) then AddFPCCFGSnippet('#IFDEF CPU'+UpperCase(SubArchName));

    // Configuration snippet for FPC
    AddFPCCFGSnippet('-FD'+BinUtilsPath);
    AddFPCCFGSnippet('-XP'+FBinUtilsPrefix); {Prepend the binutils names};

    if (FSubArch=TSUBARCH.lx6) then AddFPCCFGSnippet('-Wpesp32',false);
    if (FSubArch=TSUBARCH.lx106) then AddFPCCFGSnippet('-Wpesp8266',false);

    ToolVersion:='';
    for S in FCrossOpts do
    begin
      if AnsiStartsStr('-WP',S) then
      begin
        ToolVersion:='-'+Copy(Trim(S),4,MaxInt);
        break;
      end;
    end;

    S:=Trim(GetEnvironmentVariable('IDF_PATH'));
    if (Length(S)=0) then
    begin
      FilePath:=ConcatPaths([BasePath,CROSSBINPATH,TargetCPUName+'-'+TargetOSName]);
      ESPToolFiles:=FindAllFiles(FilePath, 'esptool.py', true);
      FilePath:='';
      try
        for S in ESPToolFiles do
        begin
          if (FSubArch=TSUBARCH.lx6) then
          begin
            if (Pos('esp-idf'+ToolVersion,S)>0) then
            begin
              FilePath:=S;
              break;
            end;
          end;
          if (FSubArch=TSUBARCH.lx106) then
          begin
            if (Pos('esp-rtos'+ToolVersion,S)>0) then
            begin
              FilePath:=S;
              break;
            end;
          end;
        end;
      finally
        ESPToolFiles.Free;
      end;
      if (Length(FilePath)>0) then
      begin
        FilePath:=ExtractFileDir(FilePath);
        repeat
          FilePath:=SafeExpandFileName(FilePath+DirectorySeparator+'..');
          S:=ExtractFileName(FilePath);
        until ((S='components') OR (Length(S)=0));
        S:=SafeExpandFileName(FilePath+DirectorySeparator+'..');
      end;
    end;

    if ((Length(S)>0) AND DirectoryExists(S)) then
    begin
      AddFPCCFGSnippet('-Ff'+S); {Set the IDF SDK path};
      if (FSubArch=TSUBARCH.lx6) then
      begin
        FilePath:=ConcatPaths([S,'components','esp_rom','esp32','ld']);
        if DirectoryExists(FilePath) then AddFPCCFGSnippet('-Fl'+FilePath);
        FilePath:=ConcatPaths([S,'components','esp32','ld']);
        if DirectoryExists(FilePath) then AddFPCCFGSnippet('-Fl'+FilePath);
      end;
      if (FSubArch=TSUBARCH.lx106) then
      begin
        FilePath:=ConcatPaths([S,'components','esp8266','ld']);
        if DirectoryExists(FilePath) then AddFPCCFGSnippet('-Fl'+FilePath);
      end;
    end;

    if (FSubArch<>TSUBARCH.saNone) then AddFPCCFGSnippet('#ENDIF CPU'+UpperCase(SubArchName));

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

