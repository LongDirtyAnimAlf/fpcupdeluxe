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
//see http://wiki.lazarus.freepascal.org/Extending_the_IDE#Load.2FSave_settings
//see my email to list on config options
//lazarus tools/win dir has an environmentoptions.xml which we might use?
{seems useful:
<LazarusDirectory Value="%LazDir%">
</LazarusDirectory>
<CompilerFilename Value="%FpcBinDir%\fpc.exe">
</CompilerFilename>
<FPCSourceDirectory Value="$(LazarusDir)fpc\$(FPCVer)\source">
</FPCSourceDirectory>
<MakeFilename Value="%FpcBinDir%\make.exe">
</MakeFilename>
<TestBuildDirectory Value="%Temp%">
</TestBuildDirectory>
<Debugger Class="TGDBMIDebugger"/>
<DebuggerFilename Value="%LazDir%\mingw\bin\gdb.exe">
</DebuggerFilename>
}
//todo: check out build-cross.bat in win dir for lazarus for crosscompiling setup instructions

{ TUpdateLazConfig }

TUpdateLazConfig = class(TObject)
private
  FConfig: TXMLConfig;
  FCompilerFilename: string;
  FDebuggerFilename: string;
  FFPCSourceDirectory: string;
  FLazarusDirectory: string;
  FMakeFilename: string;
  FPrimaryConfigPath: string;
  FTestBuildDirectory: string;
public
  {New compiler filename. May include macros, except FPCVer. If empty, use current/default value}
  property CompilerFilename: string read FCompilerFilename write FCompilerFilename;
  {New debugger filename. May include macros. If empty, use current/default value}
  property DebuggerFilename: string read FDebuggerFilename write FDebuggerFilename;
  {New FPC source directory. May include macros. If empty, use current/default value}
  property FPCSourceDirectory: string read FFPCSourceDirectory write FFPCSourceDirectory;
  {New Lazarus directory. May NOT include macros. If empty, use current/default value}
  property LazarusDirectory: string read FLazarusDirectory write FLazarusDirectory;
  {New make filename. May include macros. If empty, use current/default value}
  property MakeFilename: string read FMakeFilename write FMakeFilename;
  {Path where Lazarus config should be created or updated}
  property PrimaryConfigPath: string read FPrimaryConfigPath;
  {New test build directory (directory for testing build options). May include macros. If empty, use current/default value}
  property TestBuildDirectory: string read FTestBuildDirectory write FTestBuildDirectory;
  {Create object; specify path (primary config path) where options should be created or updated}
  constructor Create(ConfigPath: string);
  destructor Destroy; override;
end;
implementation

{ TUpdateLazConfig }
const
  ConfigFile='environmentoptions.xml';

constructor TUpdateLazConfig.Create(ConfigPath: string);
begin
  FPrimaryConfigPath:=IncludeTrailingPathDelimiter(ConfigPath)+ConfigFile;
  FConfig:=TXMLConfig.Create(FPrimaryConfigPath);
end;

destructor TUpdateLazConfig.Destroy;
begin
  //todo: create settings with defaults if new file.
  //todo: add Debugger Class TGDBMIDebugger
  //todo: add Version Value 106
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

