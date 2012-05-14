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
{ Creates or updates Lazarus configs (or XML files).
Can handle arbitrary number of config files.
Specify filename only if you want to save in the config path set in the Create constructor; else specify path and filename.
Will save all configs when it is destroyed.

Note: if you pass a variable such as Help#, this will cause an exception in the XML writing code that is called by laz2_xmlcfg
I'm not adding error protection here, as we should not write those kinds of variables; if we do, I'd like to see the error in the calling module.
}

{ Changes in v1.0 Lazarus config versus earlier config:
Ran fpcup without settings, then ran Lazarus 1.1 (trunk) and converted to new format.
After conversion, probably quite a lot of default settings were filled in by the IDE when saving for the first time.
Therefore the difference listed below may be exaggerated.
1. Environmentoptions.xml
- Version 106=>107, add Lazarus="1.1" (or 1.0 or whatever) attribute to version
- adds history count... list to LazarusDirectory, CompilerFilename, MakeFilename
- adds a lot of new elements with children: Desktop, PseudoTerminal, Watches, BreakPoints, Locals, CallStack...
- adds ObjectInspectorOptions section after EnvironmentOptions
2. FPCDefines.xml
- for some strange reason, in this line
3. Newly created files - probably default settings:
- editoroptions.xml
- includelinks.xml
- inputhistory.xml
- laz_indentation.xml
- lazarus.dci
- projectsessions directory
4. Deleted file: compilertest.pas does not exist in the new verison.
RealCompiler File="C:\development\fpc\bin\i386-win32\ppc386.exe"
I now get InPath="C:\development\fpcbootstrap\ppc386.exe" instead of the fpc\bin path

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz_DOM, Laz_XMLRead, Laz_XMLWrite;
const
  // Some fixed configuration files.
  // General options:
  EnvironmentConfig='environmentoptions.xml';
  // Help optons:
  HelpConfig='helpoptions.xml';
  // Packages:
  PackageConfig='packagefiles.xml';
  // Versions used when new config files are generated.
  // We can assume Lazarus SVN can parse this version:
  // Lazarus pre 1.0: 106
  VersionNewEnvironmentConfig='107';
  VersionNewHelpConfig='1';
  VersionNewPackageConfig='2';

type

{ TConfigVariable }

TConfig=class; //forward declaration
TUpdateLazConfig=class; //forward declaration

TConfig = class(TObject)
private
  bChanged:boolean;
  Filename:string;
  FNew: boolean;
  Doc:TXMLDocument;
protected
  // Did the config file exist before using it?
  property New: boolean read FNew;
  // Delete a child from a different part of the tree
  procedure DeletePath(OldPath: string);
  procedure DeleteValue(const APath: string);
  function FindNode(APath: string;var AttrName:string;bCreate:boolean):TDomNode;
  function GetValue(const APath, ADefault: String): String;
  function GetValue(const APath: String; ADefault: Integer): Integer;
  function GetValue(const APath: String; ADefault: Boolean): Boolean;
  // Move a child from a different part of the tree
  procedure MovePath(OldPath, NewPath: string);
  // Save our changes to the config variable
  procedure Save;
  procedure SetValue(const APath, AValue: String);
  procedure SetValue(const APath: String; AValue: Integer);
  procedure SetValue(const APath: String; AValue: Boolean);

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
  { Remove path leading up to a variable (e.g. EnvironmentOptions/ExternalTools) }
  procedure DeletePath(ConfigFile, Path: string);
  { Remove entire variable }
  procedure DeleteVariable(ConfigFile, Variable:string);
  { Returns string variable content, or empty string if it doesn't exist }
  function GetVariable(ConfigFile, Variable: string): string;
  { Returns integer variable content, or Default if it doesn't exit }
  function GetVariable(ConfigFile, Variable: string; Default: integer): integer;
  { Returns boolean variable content, or Default if it doesn't exit }
  function GetVariable(ConfigFile, Variable: string; Default: boolean): boolean;
  { Move part of the tree to another part of the tree: can be used to move children }
  procedure MovePath(ConfigFile, OldPath, NewPath: string);
  { Sets string variable to a certain value.}
  procedure SetVariable(ConfigFile, Variable, Value: string);
  { Sets integer variable to a certain value}
  procedure SetVariable(ConfigFile, Variable: string; Value: integer);
  { Sets boolean variable to a certain value}
  procedure SetVariable(ConfigFile, Variable: string; Value: boolean);
  { Sets variable to a certain value, only if a config file is created for us.}
  procedure SetVariableIfNewFile(ConfigFile, Variable, Value: string);
  {Create object; specify path (primary config path) where option files should be created or updated:}
  constructor Create(ConfigPath: string);
  destructor Destroy; override;
end;
implementation
uses FileUtil;

{ TConfig }

procedure TConfig.DeletePath(OldPath: string);
var
  OldChild: TDOMNode;
  AttrName: string;
begin
  if OldPath[length(OldPath)]='/' then
    SetLength(OldPath,length(OldPath)-1);
  OldChild:=FindNode(OldPath+'/blah',AttrName,false); // add dummy attribute to path
  if not Assigned(OldChild) then
    exit;
  OldChild.ParentNode.RemoveChild(OldChild);
  bChanged:=true;
end;

procedure TConfig.DeleteValue(const APath: string);
var
  Node: TDomNode;
  AttrName: string;
begin
  Node:=FindNode(APath,AttrName,false);
  if Node=nil then
    exit;
  if Assigned(TDOMElement(Node).GetAttributeNode(AttrName)) then begin
    begin
    TDOMElement(Node).RemoveAttribute(AttrName);
    bChanged:=true;
    end;
  end;
end;

function TConfig.FindNode(APath: string;var AttrName:string;bCreate:boolean): TDomNode;
var
  Node,Parent: TDOMNode;
  NodeName: String;
  StartPos: integer;
begin
  result:=nil;
  AttrName:='';
  Node:=Doc.FindNode('CONFIG');
  while assigned(Node) and (pos('/',APath)>0) do //walk in tree until no more /
    begin
    NodeName:=copy(APath,1,pos('/',APath)-1);
    Delete(APath,1,length(NodeName)+1);
    Parent:=Node;
    Node:=Node.FindNode(NodeName);
    if not assigned(Node) and bCreate then
      begin
      Node:=Doc.CreateElement(NodeName);
      Parent.AppendChild(Node);
      end;
    end;
  if assigned(Node) then
    begin
    AttrName:=APath;
    result:=Node;
    end;
end;

function TConfig.GetValue(const APath, ADefault: String): String;
var
  Node, Attr: TDOMNode;
  AttrName: String;
  StartPos: integer;
begin
  Result:=ADefault;
  Node:=FindNode(APath,AttrName,false);
  if Node=nil then
    exit;
  Attr := Node.Attributes.GetNamedItem(AttrName);
  if Assigned(Attr) then
    Result := Attr.NodeValue;
end;

function TConfig.GetValue(const APath: String; ADefault: Integer): Integer;
begin
  Result := StrToIntDef(GetValue(APath, IntToStr(ADefault)),ADefault);
end;

function TConfig.GetValue(const APath: String; ADefault: Boolean): Boolean;
var
  s: String;
begin
  if ADefault then
    s := 'True'
  else
    s := 'False';

  s := GetValue(APath, s);

  if CompareText(s,'TRUE')=0 then
    Result := True
  else if CompareText(s,'FALSE')=0 then
    Result := False
  else
    Result := ADefault;
end;

procedure TConfig.MovePath(OldPath, NewPath: string);
var
  NewChild, OldChild,Parent: TDOMNode;
  AttrName:string;
  i:integer;
begin
  if NewPath[length(NewPath)]='/' then
    SetLength(NewPath,length(NewPath)-1);
  if OldPath[length(OldPath)]='/' then
    SetLength(OldPath,length(OldPath)-1);
  NewChild:=FindNode(NewPath+'/blah',AttrName,false);  // append dummy attribute to path
  OldChild:=FindNode(OldPath+'/bloh',AttrName,false);
  while Assigned(NewChild.FirstChild) do
    NewChild.RemoveChild(NewChild.FirstChild);
  for i:=0 to OldChild.ChildNodes.Count-1 do
    begin
    NewChild.AppendChild(OldChild.ChildNodes.Item[i].CloneNode(True));
    end;
  bChanged:=true;
end;

procedure TConfig.Save;
begin
  WriteXMLFile(Doc,Filename);
end;

procedure TConfig.SetValue(const APath, AValue: String);
var
  Node: TDOMNode;
  AttrName: String;
  StartPos: integer;
begin
  Node:=FindNode(APath,AttrName,true);
  if Node=nil then
    exit;
  if (not Assigned(TDOMElement(Node).GetAttributeNode(AttrName))) or
    (TDOMElement(Node)[AttrName] <> AValue) then
  begin
    TDOMElement(Node)[AttrName] := AValue;
    bChanged:=true;
  end;
end;

procedure TConfig.SetValue(const APath: String; AValue: Integer);
begin
  SetValue(APath, IntToStr(AValue));
end;

procedure TConfig.SetValue(const APath: String; AValue: Boolean);
begin
  if AValue then
    SetValue(APath, 'True')
  else
    SetValue(APath, 'False');
end;

constructor TConfig.Create(const AFilename: String);
var
  FileOnly: string;
begin
  Filename:=AFilename;
  FNew:=not(FileExistsUTF8(AFileName));
  if FNew then
    begin
    Doc:=TXMLDocument.Create;
    // CONFIG node present in all Lazarus configs=>we ensure the config file gets created if it doesn't exist yet:
    Doc.AppendChild(Doc.CreateElement('CONFIG'));
    end
  else
    ReadXMLFile(Doc,AFilename);
  bChanged:=false;
end;

destructor TConfig.Destroy;
begin
  If bChanged then
    Save;
  Doc.Free;
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

procedure TUpdateLazConfig.DeletePath(ConfigFile, Path: string);
var
  Config: TConfig;
  VariableIndex: integer;
begin
  Config:=GetConfig(ConfigFile);
  Config.DeletePath(Path);
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
    if NewConfig.New then
    begin
      // Set up default config, including version number
      case (ExtractFileName(ConfigFile)) of
        EnvironmentConfig:
          begin
          NewConfig.SetValue('EnvironmentOptions/Version/Value', VersionNewEnvironmentConfig);
          // If we don't add this, we trigger an upgrade process on first start on Lazarus 1.1+.
          // We don't know what version we're downloading so just use 1.1
          NewConfig.SetValue('EnvironmentOptions/Version/Lazarus', '1.1');
          end;
        HelpConfig: NewConfig.SetValue('HelpOptions/Version/Value', VersionNewHelpConfig);
        PackageConfig:
          begin
            // Note: Version= in this file is an attribute of UserPkgLinks; might not matter
            NewConfig.SetValue('UserPkgLinks/Version', VersionNewPackageConfig);
            NewConfig.SetValue('UserPkgLinks/Count', '0');
          end;
      end;
    end;
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

function TUpdateLazConfig.GetVariable(ConfigFile, Variable: string): string;
var
  Config: TConfig;
begin
  // Don't free this one, as it will remove it from the list
  Config:=GetConfig(ConfigFile);
  result:=Config.GetValue(Variable, '');
end;

function TUpdateLazConfig.GetVariable(ConfigFile, Variable: string;
  Default: integer): integer;
var
  Config: TConfig;
begin
  // Don't free this one, as it will remove it from the list
  Config:=GetConfig(ConfigFile);
  result:=Config.GetValue(Variable, Default);
end;

function TUpdateLazConfig.GetVariable(ConfigFile, Variable: string;
  Default: boolean): boolean;
var
  Config: TConfig;
begin
  // Don't free this one, as it will remove it from the list
  Config:=GetConfig(ConfigFile);
  result:=Config.GetValue(Variable, Default);
end;

procedure TUpdateLazConfig.MovePath(ConfigFile, OldPath, NewPath: string);
var
  Config: TConfig;
begin
  Config:=GetConfig(ConfigFile);
  Config.MovePath(OldPath, NewPath);
end;

procedure TUpdateLazConfig.SetVariable(ConfigFile, Variable, Value: string);
var
  Config: TConfig;
begin
  // Don't free this one, as it will remove it from the list
  Config:=GetConfig(ConfigFile);
  Config.SetValue(Variable, Value);
end;

procedure TUpdateLazConfig.SetVariable(ConfigFile, Variable: string;
  Value: integer);
var
  Config: TConfig;
begin
  // Don't free this one, as it will remove it from the list
  Config:=GetConfig(ConfigFile);
  Config.SetValue(Variable, Value);
end;

procedure TUpdateLazConfig.SetVariable(ConfigFile, Variable: string;
  Value: boolean);
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

