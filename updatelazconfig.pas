{ FPC/Lazarus installer/updater
Copyright (C) 2012 Reinier Olislagers

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


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, laz2_xmlcfg;
type
{ Creates or updates Lazarus config in primary config path given.}


{ TUpdateLazConfig }

TUpdateLazConfig = class(TObject)
private
  FConfig: TXMLConfig;
  FCompilerFilename: string;
  FDebuggerFilename: string;
  FFPCSourceDirectory: string;
  FIsNewFile: boolean;
  FLazarusDirectory: string;
  FMakeFilename: string;
  FConfigFile: string;
  FTestBuildDirectory: string;
public
  {New compiler filename. May include macros, except FPCVer. If empty, use current/default value:}
  property CompilerFilename: string read FCompilerFilename write FCompilerFilename;
  {Config file being created/updated:}
  property ConfigFile: string read FConfigFile;
  {New debugger filename. May include macros. If empty, use current/default value:}
  property DebuggerFilename: string read FDebuggerFilename write FDebuggerFilename;
  {New FPC source directory. May include macros. If empty, use current/default value:}
  property FPCSourceDirectory: string read FFPCSourceDirectory write FFPCSourceDirectory;
  {New Lazarus directory. May NOT include macros. If empty, use current/default value:}
  property LazarusDirectory: string read FLazarusDirectory write FLazarusDirectory;
  {New make filename. May include macros. If empty, use current/default value:}
  property MakeFilename: string read FMakeFilename write FMakeFilename;
  {Is this a new config file or an existing one?}
  property New: boolean read FIsNewFile;
  {New test build directory (directory for testing build options). May include macros. If empty, use current/default value:}
  property TestBuildDirectory: string read FTestBuildDirectory write FTestBuildDirectory;
  {Create object; specify path (primary config path) where options should be created or updated:}
  constructor Create(ConfigPath: string);
  destructor Destroy; override;
end;
implementation
uses FileUtil;

{ TUpdateLazConfig }
const
  ConfigFile='environmentoptions.xml';
  VersionNewConfig='106'; //We can assume Lazarus SVN can parse this version

constructor TUpdateLazConfig.Create(ConfigPath: string);
begin
  FConfigFile:=IncludeTrailingPathDelimiter(ConfigPath)+ConfigFile;
  // Assume any file that exists is also valid... might be improved by checking
  // for correct values.
  if FileExistsUTF8(FConfigFile) then FIsNewFile:=false else FIsNewFile:=true;
  FConfig:=TXMLConfig.Create(FConfigFile);
end;

destructor TUpdateLazConfig.Destroy;
begin
  //todo: create settings with defaults if new file.
  if New then
  begin
    // Set up some sensible defaults
    FConfig.SetValue('EnvironmentOptions/Version/Value', VersionNewConfig);
    FConfig.SetValue('EnvironmentOptions/Debugger/Class','TGDBMIDebugger');
    FConfig.SetValue('EnvironmentOptions/DebuggerFilename/Value', 'gdb'); //assume in path
    {$IFDEF WINDOWS}
    FConfig.SetValue('EnvironmentOptions/CompilerFilename/Value', '%FpcBinDir%\fpc.exe');
    FConfig.SetValue('EnvironmentOptions/FPCSourceDirectory/Value', '$(LazarusDir)fpc\$(FPCVer)\source');
    FConfig.SetValue('EnvironmentOptions/LazarusDirectory/Value', 'c:\lazarus');
    FConfig.SetValue('EnvironmentOptions/MakeFilename/Value', '%FpcBinDir%\make.exe');
    FConfig.SetValue('EnvironmentOptions/TestBuildDirectory/Value', '%Temp%');
    {$ENDIF WINDOWS}
    {$IFDEF UNIX}
    FConfig.SetValue('EnvironmentOptions/CompilerFilename/Value', '/usr/bin/fpc');
    FConfig.SetValue('EnvironmentOptions/FPCSourceDirectory/Value', '/usr/share/fpcsrc/$(FPCVer)/fpc/');
    FConfig.SetValue('EnvironmentOptions/LazarusDirectory/Value', '/usr/share/lazarus');
    FConfig.SetValue('EnvironmentOptions/MakeFilename/Value', 'make'); //assume in path
    FConfig.SetValue('EnvironmentOptions/TestBuildDirectory/Value', '/tmp');
    {$ENDIF UNIX}
    //todo: check more architectures
  end;
  if CompilerFileName<>Emptystr then FConfig.SetValue('EnvironmentOptions/CompilerFilename/Value', CompilerFileName);
  if DebuggerFilename<>Emptystr then FConfig.SetValue('EnvironmentOptions/DebuggerFilename/Value', DebuggerFilename);
  if FPCSourceDirectory<>Emptystr then FConfig.SetValue('EnvironmentOptions/FPCSourceDirectory/Value', FPCSourceDirectory);
  if LazarusDirectory<>Emptystr then FConfig.SetValue('EnvironmentOptions/LazarusDirectory/Value', LazarusDirectory);
  if MakeFilename<>Emptystr then FConfig.SetValue('EnvironmentOptions/MakeFilename/Value', MakeFilename);
  if TestBuildDirectory<>Emptystr then FConfig.SetValue('EnvironmentOptions/TestBuildDirectory/Value', TestBuildDirectory);
  FConfig.Flush; //write out newly created or updated file
  FConfig.Free;
  inherited Destroy;
end;


end.

