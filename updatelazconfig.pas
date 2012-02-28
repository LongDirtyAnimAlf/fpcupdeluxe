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
const
  EnvironmentConfig='environmentoptions.xml';
  HelpConfig='helpoptions.xml';
  VersionNewConfig='106'; //We can assume Lazarus SVN can parse this version
  VersionNewHelpConfig='1'; //Use this version in our help config file

type

{ TConfigVariable }

TConfig=class; //forward declaration
TUpdateLazConfig=class; //forward declaration

TConfig = class(TXMLConfig)
private
  FDirty: boolean;
  FNew: boolean;
protected
  //todo: check if this will work.
  procedure AddChild(ParentPath, ChildVar, ChildValue: string); unimplemented;
  // Counts subunits. Uses convention: parent name is stripped child name +s
  // Child name=stripped child name+number
  // E.g. Units=>Unit0,Unit1.. etc
  function CountChildren(ParentPath: string): integer; unimplemented;
  // Did the config file exist before using it?
  property New: boolean read FNew;
  // Save our changes to the config variable
  procedure Save;
public
  constructor Create(const AFilename: String); overload; // create and load
  destructor Destroy; override;
end;

{ TUpdateLazConfig }
TUpdateLazConfig = class(TObject)
private
  //List of TConfigs, with absolute file names
  FConfigs: TStringList;
  // Place where config files stores if no path component given
  FDefaultConfigPath: string;
  function GetConfig(const ConfigFile: string): TConfig;
  procedure WriteConfig;
public
  { Remove entire variable }
  procedure DeleteVariable(ConfigFile, Variable:string);
  { Sets variable to a certain value.}
  procedure SetVariable(ConfigFile, Variable, Value: string);
  { Sets variable to a certain value, only if a config file is created for us.}
  procedure SetVariableIfNewFile(ConfigFile, Variable, Value: string);
  {Create object; specify path (primary config path) where option files should be created or updated:}
  constructor Create(ConfigPath: string);
  destructor Destroy; override;
end;
implementation
uses FileUtil;

{ TConfig }

procedure TConfig.AddChild(ParentPath, ChildVar, ChildValue: string); unimplemented;
begin
//todo: implement. Note procedure signature may change depending on needs
end;

function TConfig.CountChildren(ParentPath: string): integer; unimplemented;
begin
  //todo: implement
end;

procedure TConfig.Save;
// Alias for flush, really..
begin
  inherited Flush;
end;

constructor TConfig.Create(const AFilename: String);
begin
  FNew:=not(FileExistsUTF8(AFileName));
  (Self as TXMLConfig).Create(AFileName);
  FDirty:=false;
end;

destructor TConfig.Destroy;
var
  Counter:integer;
begin
  // The destroy will call flush to save
  // the config...
  inherited Destroy;
end;


procedure TUpdateLazConfig.WriteConfig;
var
  Counter:integer;
begin
  // Write all configs to disk if necessary
  for Counter:=0 to FConfigs.Count-1 do
  begin
   (FConfigs.Objects[Counter] As TConfig).Save;
  end;
end;

function TUpdateLazConfig.GetConfig(const ConfigFile: string): TConfig;
var
  ConfigIndex: integer;
  FileName: string;
  NewConfig: TConfig;
begin
  if ExtractFileName(ConfigFile)=Configfile then
  begin
    // No directory given, place in config path
    FileName:=ExpandFileName(IncludeTrailingPathDelimiter(FDefaultConfigPath)+
      ConfigFile);
  end
  else
  begin
    // Normalize
    FileName:=ExpandFileName(ConfigFile);
  end;
  ConfigIndex:=FConfigs.IndexOf(FileName);
  if ConfigIndex=-1 then
  begin
    NewConfig:=TConfig.Create(FileName);
    ConfigIndex:=FConfigs.AddObject(FileName, NewConfig);
    //NewConfig.Free; //This would remove object from stringlist
  end;
  Result:=(FConfigs.Objects[ConfigIndex] As TConfig);
end;

procedure TUpdateLazConfig.DeleteVariable(ConfigFile, Variable:string);
var
  Config: TConfig;
  VariableIndex: integer;
begin
  Config:=GetConfig(ConfigFile);
  Config.DeleteValue(Variable);
end;

procedure TUpdateLazConfig.SetVariable(ConfigFile, Variable, Value: string);
var
  Config: TConfig;
begin
  // Don't free this one, as it will remove it from the list
  Config:=GetConfig(ConfigFile);
  Config.SetValue(Variable, Value);
end;

procedure TUpdateLazConfig.SetVariableIfNewFile(ConfigFile, Variable,
  Value: string);
var
  Config: TConfig;
begin
  // Don't free this one, as it will remove it from the list
  Config:=GetConfig(ConfigFile);
  if Config.New then Config.SetValue(Variable, Value);
end;

constructor TUpdateLazConfig.Create(ConfigPath: string);
begin
  FConfigs:=TStringList.Create;
  FConfigs.Sorted:=true;
  FConfigs.Duplicates:=dupError;
  FDefaultConfigPath:=IncludeTrailingPathDelimiter(ExpandFileName(ConfigPath));
end;

destructor TUpdateLazConfig.Destroy;
var
  Counter: integer;
begin
  // When FConfigs go out of scope, they save to file first...
  for Counter:=0 to FConfigs.Count-1 do
  begin
    FConfigs.Objects[Counter].Free;
  end;
  FConfigs.Free;
  inherited Destroy;
end;


end.

