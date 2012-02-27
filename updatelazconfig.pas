{ FPC/Lazarus installer/updater
Copyright (C) 2012 Reinier Olislagers, Ludo Brands

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

unit updatelazconfig;
{ Creates or updates Lazarus config in primary config path given.}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, laz2_xmlcfg;
type
//todo: append as needed
TConfigVariable=(VCHMHelpFilesPath);

{ TUpdateLazConfig }
TUpdateLazConfig = class(TObject)
private
  FCHMHelpExe: string;
  FCHMHelpFilesPath: string;
  FConfig: TXMLConfig;
  // List of settings that need to be deleted for the help section
  FHelpDeleteList: TStringlist;
  FHelpConfig: TXMLConfig;
  FCompilerFilename: string;
  FDebuggerFilename: string;
  FFPCSourceDirectory: string;
  FNewHelpFile: boolean;
  FNewMainFile: boolean;
  FLazarusDirectory: string;
  FMakeFilename: string;
  FConfigFile: string;
  FHelpConfigFile: string;
  FTestBuildDirectory: string;
public
  {Path to CHM help viewer, such as lhelp.exe}
  property CHMHelpExe: string read FCHMHelpExe write FCHMHelpExe;
  {Directory where CHM files are searched. If you want to use subdirectories, modify the baseURL settings for the individual files.}
  property CHMHelpFilesPath: string read FCHMHelpFilesPath write FCHMHelpFilesPath;
  {New compiler filename. May include macros, except FPCVer. If empty, use current/default value:}
  property CompilerFilename: string read FCompilerFilename write FCompilerFilename;
  {Config file being created/updated:}
  property ConfigFile: string read FConfigFile;
  {New debugger filename. May include macros. If empty, use current/default value:}
  property DebuggerFilename: string read FDebuggerFilename write FDebuggerFilename;
  {Removes entire entry for configuration setting. This will OVERRIDE any variable set via the other properties}
  procedure DeleteVariable(Variable:TConfigVariable);
  {New FPC source directory. May include macros. If empty, use current/default value:}
  property FPCSourceDirectory: string read FFPCSourceDirectory write FFPCSourceDirectory;
  {New Lazarus directory. May NOT include macros. If empty, use current/default value:}
  property LazarusDirectory: string read FLazarusDirectory write FLazarusDirectory;
  {NewMainFile make filename. May include macros. If empty, use current/default value:}
  property MakeFilename: string read FMakeFilename write FMakeFilename;
  {Is this a new help config file or an existing one?}
  property NewHelpFile: boolean read FNewHelpFile;
  {Is this a new config file or an existing one?}
  property NewMainFile: boolean read FNewMainFile;
  {New test build directory (directory for testing build options). May include macros. If empty, use current/default value:}
  property TestBuildDirectory: string read FTestBuildDirectory write FTestBuildDirectory;
  {Create object; specify path (primary config path) where option files should be created or updated:}
  constructor Create(ConfigPath: string);
  destructor Destroy; override;
end;
implementation
uses FileUtil;

{ TUpdateLazConfig }
const
  ConfigFileName='environmentoptions.xml';
  HelpConfigFileName='helpoptions.xml';
  VersionNewConfig='106'; //We can assume Lazarus SVN can parse this version
  VersionNewHelpConfig='1'; //Use this version in our help config file

procedure TUpdateLazConfig.DeleteVariable(Variable: TConfigVariable);
// Deleted values override set values regardless of the order
// the property/procedure is called
// todo: rewrite class to use a dictionary or similar that maps
// config names to strings used in FConfig.setvalue/deletevalue or
// have a single list for both add and delete...
begin
  case Variable of
    VCHMHelpFilesPath: FHelpDeleteList.Add('Viewers/TChmHelpViewer/CHMHelp/FilesPath');
  end;
end;

constructor TUpdateLazConfig.Create(ConfigPath: string);
begin
  FConfigFile:=IncludeTrailingPathDelimiter(ConfigPath)+ConfigFileName;
  FHelpConfigFile:=IncludeTrailingPathDelimiter(ConfigPath)+HelpConfigFileName;
  // Assume any file that exists is also valid... might be improved by checking
  // for correct values.
  if FileExistsUTF8(FConfigFile) then FNewMainFile:=false else FNewMainFile:=true;
  if FileExistsUTF8(FHelpConfigFile) then FNewHelpFile:=false else FNewHelpFile:=true;
  FConfig:=TXMLConfig.Create(FConfigFile);
  FHelpConfig:=TXMLConfig.Create(FHelpConfigFile);
  FHelpDeleteList:=TStringList.Create;
end;

destructor TUpdateLazConfig.Destroy;
var
  DeleteCounter:integer;
begin
  try
    if NewMainFile then
    begin
      // Set up some sensible defaults
      FConfig.SetValue('EnvironmentOptions/Version/Value', VersionNewConfig);
      FConfig.SetValue('EnvironmentOptions/Debugger/Class','TGDBMIDebugger');
      {$IFDEF MSWINDOWS}
      FConfig.SetValue('EnvironmentOptions/CompilerFilename/Value', '%FpcBinDir%\fpc.exe');
      FConfig.SetValue('EnvironmentOptions/DebuggerFilename/Value', 'gdb.exe'); //assume in path
      FConfig.SetValue('EnvironmentOptions/FPCSourceDirectory/Value', '$(LazarusDir)fpc\$(FPCVer)\source');
      FConfig.SetValue('EnvironmentOptions/LazarusDirectory/Value', 'c:\lazarus');
      FConfig.SetValue('EnvironmentOptions/MakeFilename/Value', '%FpcBinDir%\make.exe');
      FConfig.SetValue('EnvironmentOptions/TestBuildDirectory/Value', GetTempDir(false));
      {$ENDIF MSWINDOWS}
      {$IFDEF UNIX}
      FConfig.SetValue('EnvironmentOptions/CompilerFilename/Value', '/usr/bin/fpc');
      FConfig.SetValue('EnvironmentOptions/DebuggerFilename/Value', 'gdb'); //assume in path
      FConfig.SetValue('EnvironmentOptions/FPCSourceDirectory/Value', '/usr/share/fpcsrc/$(FPCVer)/fpc/');
      FConfig.SetValue('EnvironmentOptions/LazarusDirectory/Value', '/usr/share/lazarus');
      FConfig.SetValue('EnvironmentOptions/MakeFilename/Value', 'make'); //assume in path
      FConfig.SetValue('EnvironmentOptions/TestBuildDirectory/Value', '/tmp');
      {$ENDIF UNIX}
    end;
    if FCompilerFileName<>Emptystr then FConfig.SetValue('EnvironmentOptions/CompilerFilename/Value', FCompilerFileName);
    if FDebuggerFilename<>Emptystr then FConfig.SetValue('EnvironmentOptions/DebuggerFilename/Value', FDebuggerFilename);
    if FFPCSourceDirectory<>Emptystr then FConfig.SetValue('EnvironmentOptions/FPCSourceDirectory/Value', FFPCSourceDirectory);
    if FLazarusDirectory<>Emptystr then FConfig.SetValue('EnvironmentOptions/LazarusDirectory/Value', FLazarusDirectory);
    if FMakeFilename<>Emptystr then FConfig.SetValue('EnvironmentOptions/MakeFilename/Value', FMakeFilename);
    if FTestBuildDirectory<>Emptystr then FConfig.SetValue('EnvironmentOptions/TestBuildDirectory/Value', FTestBuildDirectory);
    FConfig.Flush; //write out newly created or updated file
  finally
    // Regardless of what happens, try to prevent memory leaks.
    FConfig.Free;
  end;
  try
    if NewHelpFile then
    begin
      // Defaults
      FConfig.SetValue('HelpOptions/Version/Value', VersionNewHelpConfig);
      // We don't know the location of the help viewer or help files
      FHelpConfig.SetValue('Viewers/TChmHelpViewer/CHMHelp/Exe', EmptyStr);
      FHelpConfig.SetValue('Viewers/TChmHelpViewer/CHMHelp/FilesPath', EmptyStr);
    end;
    if FCHMHelpExe<>EmptyStr then FHelpConfig.SetValue('Viewers/TChmHelpViewer/CHMHelp/Exe', FCHMHelpExe);
    if FCHMHelpFilesPath<>EmptyStr then FHelpConfig.SetValue('Viewers/TChmHelpViewer/CHMHelp/FilesPath', FCHMHelpFilesPath);
    // Deleted items override set items for now...
    for DeleteCounter:=0 to FHelpDeleteList.Count-1 do
    begin
      FHelpConfig.DeleteValue(FHelpDeleteList[DeleteCounter]);
    end;
    { Writing a semantically empty xml file shouldn't hurt,
    so no checks for dirty file needed... }
    FHelpConfig.Flush;
  finally
    FHelpConfig.Free;
    FHelpDeleteList.Free;
  end;
  inherited Destroy;
end;


end.

