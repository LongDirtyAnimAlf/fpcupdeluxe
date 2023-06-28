{ FPC/Lazarus installer/updater
Copyright (C) 2012-2013 Reinier Olislagers, Ludo Brands

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
{
Creates or updates Lazarus configs (or XML files).
Can handle arbitrary number of config files.
Specify filename only if you want to save in the config path set in the Create constructor; else specify path and filename.
Will save all configs when it is destroyed.

Note: if you pass a variable such as Help#, this will cause an exception in the XML writing code that is called by laz2_xmlcfg
No error protection here, as we should not write those kinds of variables; if we do, I'd like to see the error in the calling module.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_XMLCfg, Laz2_DOM;

const
  // Some fixed configuration files.
  StaticsConfig='staticpackages.inc';
  // Editor options:
  EditorConfig='editoroptions.xml';
  // General options:
  EnvironmentConfig='environmentoptions.xml';
  // Help optons:
  HelpConfig='helpoptions.xml';
  // Packages:
  PackageConfig='packagefiles.xml';
  // Miscellaneous Options
  MiscellaneousConfig='miscellaneousoptions.xml';
  // FPC defines (source cache):
  FPCDefines='fpcdefines.xml';
  // File history:
  History='inputhistory.xml';
  // Pas2js configuration options:
  Pas2jsConfig='pas2jsdsgnoptions.xml';
  // Simple webserver options:
  WebserverConfig='simplewebservergui.xml';
  // BuildIDE config file
  DefaultIDEMakeOptionFilename='idemake.cfg';

type

{ TConfigVariable }

TConfig=class; //forward declaration
TUpdateLazConfig=class; //forward declaration

TConfig = class(TXMLConfig)
private
  FNew:boolean;
public
  constructor Create(const AFilename: String);
  procedure Save;
  procedure MovePath(OldPath, NewPath: string);
  property New:boolean read FNew;
end;


{ TUpdateLazConfig }
TUpdateLazConfig = class(TObject)
private
  //List of TConfigs, with absolute file names
  FConfigs: TStringList;
  // Place where config files are stored if no path component given
  FDefaultConfigPath: string;
  FLazarusMajorVer: integer; //major part of the version number, e.g. 1 for 1.0.8, or -1 if unknown
  FLazarusMinor: integer; //minor part of the version number, e.g. 0 for 1.0.8, or -1 if unknown
  FLazarusRelease: integer; //release part of the version number, e.g. 8 for 1.0.8, or -1 if unknown
  FLazarusPatch: integer; //candidate part of the version number, e.g. 2 for 1.0.8RC2, or -1 if unknown
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
  { Is config file is created for us.}
  function IfNewFile(ConfigFile:string):boolean;
  { Sets variable to a certain value, only if a config file is created for us.}
  procedure SetVariableIfNewFile(ConfigFile, Variable, Value: string);
  function IsLegacyList(ConfigFile, Variable: string):boolean;
  function GetListItemCount(const ConfigFile, APath, AItemName: string; const aLegacyList: Boolean): Integer;
  function GetListItemXPath(const ConfigFile, AName: string; const AIndex: Integer; const aLegacyList: Boolean;
      const aLegacyList1Based: Boolean = False): string;
  { Create object; specify
  path (primary config path) where option files should be created or updated
  Lazarus major, minor and release version that is downloaded (or -1 if unknown
  in which case it's assumed to be latest trunk)}
  constructor Create(ConfigPath: string;
    LazarusMajorVersion: integer=-1;
    LazarusMinorVersion: integer=-1;
    LazarusReleaseVersion: integer=-1;
    LazarusPatchVersion: integer=-1);
  destructor Destroy; override;
end;

procedure LazDocPathAdd(const PathToAdd: string; LazarusConfig: TUpdateLazConfig); //Add a path to the LazDoc/fpcdoc list

implementation

uses
  installerCore,fpcuputil;

const
  // Versions used when new config files are generated.
  // Lazarus pre 1.0: 106
  // We can assume Lazarus trunk can parse this version:
  TrunkVersionNewEnvironmentConfig='110';
  TrunkLazarusNewEnvironmentConfig=LAZARUSTRUNKVERSION;
  // We use a hardcoded version for Lazarus below
  VersionNewHelpConfig='1';
  VersionNewPackageConfig='3';

procedure LazDocPathAdd(const PathToAdd: string; LazarusConfig: TUpdateLazConfig);
var
  CleanedPath: string;
  FoundIt: boolean;
  i: integer;
  TempList: TStringList;
  key,LazDocPath,xmlfile: string;
begin
  if PathToAdd<>'' then
  begin
    // Normalize path so we can compare:
    CleanedPath:=ExcludeTrailingPathDelimiter(SafeExpandFileName(PathToAdd));
    FoundIt:=false;
    xmlfile:=EnvironmentConfig;
    key:='EnvironmentOptions/LazDoc/Paths';
    LazDocPath:=LazarusConfig.GetVariable(xmlfile, key);
    // In an empty config, we just add our CleanedPath.
    // If it's not empty, we need to check if the config already contains our path:
    if LazDocPath<>'' then
    begin
      TempList:=TStringList.Create;
      try
        // Analyze all paths specified
        TempList.Delimiter:=';';
        TempList.StrictDelimiter:=True;
        TempList.DelimitedText:=LazDocPath;
        // Normalize all paths stored in setting and look for our value:
        for i := 0 to TempList.Count - 1 do
        begin
          TempList[i]:=ExcludeLeadingPathDelimiter(SafeExpandFileName(TempList[i]));
          if TempList[i]=CleanedPath then
            begin
              // Settings already include this dir
              FoundIt:=true;
              break;
            end;
        end;
        // Only add our setting if not already found
        if FoundIt then
          CleanedPath:=LazDocPath
        else
          CleanedPath:=CleanedPath+';'+LazDocPath;
      finally
        TempList.Free;
      end;
    end;
    LazarusConfig.SetVariable(xmlfile, key, CleanedPath);
  end;
end;

procedure TConfig.Save;
begin
  WriteXMLFile(Doc,Filename);
end;

constructor TConfig.Create(const AFilename: String);
begin
  FNew:=not(FileExists(aFileName));
  try
    FileName:=aFileName;
  except
    //on EXMLReadError do
    //  Exit;//eat XML exceptions
  end;
end;

procedure TConfig.MovePath(OldPath, NewPath: string);
var
  NewChild, OldChild: TDOMNode;
  i:integer;
begin
  if NewPath[length(NewPath)]='/' then
    SetLength(NewPath,length(NewPath)-1);
  if OldPath[length(OldPath)]='/' then
    SetLength(OldPath,length(OldPath)-1);
  NewChild:=FindNode(NewPath+'/blah',false);  // append dummy attribute to path
  OldChild:=FindNode(OldPath+'/bloh',false);
  while Assigned(NewChild.FirstChild) do
    NewChild.RemoveChild(NewChild.FirstChild);
  for i:=0 to OldChild.ChildNodes.Count-1 do
    begin
    NewChild.AppendChild(OldChild.ChildNodes.Item[i].CloneNode(True));
    end;
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
begin
  Config:=GetConfig(ConfigFile);
  Config.DeletePath(Path);
end;

function TUpdateLazConfig.GetConfig(const ConfigFile: string): TConfig;
var
  ConfigIndex: integer;
  FileName: string;
  NewConfig: TConfig;
  Version:string;
begin
  if ExtractFileName(ConfigFile)=Configfile then
  begin
    // No directory given, place in config path
    FileName:=SafeExpandFileName(IncludeTrailingPathDelimiter(FDefaultConfigPath)+
      ConfigFile);
  end
  else
  begin
    // Normalize
    if ConfigFile='' then
      raise Exception.Create('GetConfig: ConfigFile may not be empty');
    FileName:=SafeExpandFileName(ConfigFile);
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
            Version:='';
            if (FLazarusMajorVer<>-1) then
            begin
              Version:=Version+IntToStr(FLazarusMajorVer);
              if (FLazarusMinor<>-1) then
              begin
                Version:=Version+'.'+IntToStr(FLazarusMinor);
                if (FLazarusRelease<>-1) then
                  Version:=Version+'.'+IntToStr(FLazarusRelease);
              end;
              if (FLazarusPatch<>-1) then
                Version:=Version+'RC'+IntToStr(FLazarusPatch);
            end
            else
            begin
              NewConfig.SetValue('EnvironmentOptions/Version/Value', TrunkVersionNewEnvironmentConfig);
              NewConfig.SetValue('EnvironmentOptions/Version/Lazarus', TrunkLazarusNewEnvironmentConfig);
            end;

            if (Length(Version)>0) then NewConfig.SetValue('EnvironmentOptions/Version/Lazarus',Version);

            Version:='';
            if (FLazarusMajorVer=0) then
            begin
              if FLazarusMinor<=0 then
                NewConfig.SetValue('EnvironmentOptions/Version/Lazarus','0.9.31');
              Version:='106';
            end
            else
            if (FLazarusMajorVer=1) then
            begin
              // See EnvOptsVersion in \lazarus\ide\packages\ideconfig\environmentopts.pp
              case FLazarusMinor of
                0 : Version := '107'; //for version 1.0
                1 : Version := '107'; //for version 1.0,1.1
                2 : Version := '108'; //for version 1.2
                3 : Version := '108'; //for version 1.3
                4 : Version := '108'; //for version 1.4
                5 : Version := '109'; //for version 1.5
                6 : Version := '109'; //for version 1.6
              else
                Version:='110';
              end
            end
            else
            if (FLazarusMajorVer=2) then
            begin
              Version:='110';
            end
            else
            if (FLazarusMajorVer>=3) then
            begin
              Version:=TrunkVersionNewEnvironmentConfig;
            end;

            if (Length(Version)>0) then NewConfig.SetValue('EnvironmentOptions/Version/Value', Version);

            {$ifdef CPUAARCH64}
            // IDE does not size correctly when set to auto
            NewConfig.SetValue('Desktops/Desktop1/AutoAdjustIDEHeight/Value', 'False');
            {$endif}

          end;

        HelpConfig:
          NewConfig.SetValue('HelpOptions/Version/Value', VersionNewHelpConfig);

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

function TUpdateLazConfig.IfNewFile(ConfigFile:string):boolean;
var
  Config: TConfig;
begin
  // Don't free this one, as it will remove it from the list
  Config:=GetConfig(ConfigFile);
  result:=Config.New;
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

function TUpdateLazConfig.IsLegacyList(ConfigFile, Variable: string):boolean;
var
  Config: TConfig;
begin
  Config:=GetConfig(ConfigFile);
  result:=Config.IsLegacyList(Variable);
end;

function TUpdateLazConfig.GetListItemCount(const ConfigFile, APath, AItemName: string; const aLegacyList: Boolean): Integer;
var
  Config: TConfig;
begin
  Config:=GetConfig(ConfigFile);
  result:=Config.GetListItemCount(APath, AItemName,aLegacyList);
end;

function TUpdateLazConfig.GetListItemXPath(const ConfigFile, AName: string; const AIndex: Integer; const aLegacyList: Boolean;
    const aLegacyList1Based: Boolean): string;
var
  Config: TConfig;
begin
  Config:=GetConfig(ConfigFile);
  result:=Config.GetListItemXPath(AName,AIndex,aLegacyList,aLegacyList1Based);
end;

constructor TUpdateLazConfig.Create(ConfigPath: string;
    LazarusMajorVersion: integer=-1;
    LazarusMinorVersion: integer=-1;
    LazarusReleaseVersion: integer=-1;
    LazarusPatchVersion: integer=-1);
begin
  FLazarusMajorVer:=LazarusMajorVersion;
  FLazarusMinor:=LazarusMinorVersion;
  FLazarusRelease:=LazarusReleaseVersion;
  FLazarusPatch:=LazarusPatchVersion;
  FConfigs:=TStringList.Create;
  FConfigs.Sorted:=true;
  FConfigs.Duplicates:=dupError;
  if ConfigPath='' then
    raise Exception.Create('TUpdateLazConfig: ConfigPath may not be empty');
  FDefaultConfigPath:=IncludeTrailingPathDelimiter(SafeExpandFileName(ConfigPath));
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

