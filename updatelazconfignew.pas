unit updatelazconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  xmlconf,DOM, XMLRead, XMLWrite;
  //XMLCfg, Laz2_DOM, Laz2_XMLRead, Laz2_XMLWrite;

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
  // BuildIDE config file
  DefaultIDEMakeOptionFilename='idemake.cfg';
  // Versions used when new config files are generated.
  // Lazarus pre 1.0: 106
  // We can assume Lazarus trunk can parse this version:
  TrunkVersionNewEnvironmentConfig='110';
  TrunkLazarusNewEnvironmentConfig='2.3.0';
  // We use a hardcoded version for Lazarus below
  VersionNewHelpConfig='1';
  VersionNewPackageConfig='3';

type

{ TConfigVariable }

TConfig=class; //forward declaration
TUpdateLazConfig=class; //forward declaration

TConfig = class(TObject)
private
  bChanged: boolean;
  FFilename: string;
  FNew: boolean;
  Doc: TXMLDocument;
public
  constructor Create(const AFilename: String); overload; // create and load
  destructor Destroy; override;
  // Did the config file exist before using it?
  property New: boolean read FNew;
  // Delete a child from a different part of the tree
  procedure DeletePath(OldPath: string);
  procedure DeleteValue(const APath: string);
  function FindNode(APath: string;var AttrName:DOMString;bCreate:boolean):TDomNode;
  function GetValue(const APath, ADefault: String): String;
  function GetValue(const APath: String; ADefault: Integer): Integer;
  function GetValue(const APath: String; ADefault: Boolean): Boolean;
  // Move a child from a different part of the tree
  procedure MovePath(OldPath, NewPath: string);
  // Save our changes to the config variable
  procedure Save;
  procedure SetValue(const APath, AValue: DOMString);
  procedure SetValue(const APath: String; AValue: Integer);
  procedure SetValue(const APath: String; AValue: Boolean);
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
  fpcuputil;

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

{ TConfig }

procedure TConfig.DeletePath(OldPath: string);
var
  OldChild: TDOMNode;
  AttrName: DOMString;
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
  AttrName: DOMString;
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

function TConfig.FindNode(APath: string;var AttrName:DOMString;bCreate:boolean): TDomNode;
var
  Node,Parent: TDOMNode;
  NodeName: DOMString;
  StartPos: integer;
begin
  result:=nil;
  AttrName:='';
  Node:=Doc.FindNode('CONFIG');
  while assigned(Node) and (pos('/',APath)>0) do //walk in tree until no more /
  begin
    NodeName:=DOMString(copy(APath,1,pos('/',APath)-1));
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
  AttrName: DOMString;
  StartPos: integer;
begin
  Result:=ADefault;
  Node:=FindNode(APath,{%H-}AttrName,false);
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
  NewChild, OldChild: TDOMNode;
  AttrName:DOMString;
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
  WriteXMLFile(Doc,FFilename);
end;

procedure TConfig.SetValue(const APath, AValue: DOMString);
var
  Node: TDOMNode;
  AttrName: DOMString;
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
begin
  FFilename:=AFilename;
  FNew:=not(FileExists(AFileName));
  if FNew then
  begin
    Doc:=TXMLDocument.Create;
    // CONFIG node present in all Lazarus configs=>we ensure the config file gets created if it doesn't exist yet:
    Doc.AppendChild(Doc.CreateElement('CONFIG'));
  end
  else
    //ReadXMLFile(Doc,AFilename,[xrfAllowLowerThanInAttributeValue,xrfAllowSpecialCharsInAttributeValue,xrfAllowSpecialCharsInComments]);
    ReadXMLFile(Doc,AFilename);
  bChanged:=false;
end;

destructor TConfig.Destroy;
begin
  If bChanged then
  begin
    // Make sure path exists:
    ForceDirectoriesSafe(ExtractFilePath(FFilename));
    Save;
  end;
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
            if FLazarusMajorVer<>-1 then
            begin
              Version:=Version+IntToStr(FLazarusMajorVer);
              if FLazarusMinor<>-1 then
              begin
                Version:=Version+'.'+IntToStr(FLazarusMinor);
                if FLazarusRelease<>-1 then
                Version:=Version+'.'+IntToStr(FLazarusRelease);
              end;
            end;
            if FLazarusPatch>0 then
            begin
              Version:=Version+'RC'+IntToStr(FLazarusPatch);
            end;

            // If we don't add these, we trigger an upgrade process on first start on Lazarus 1.1+.
            NewConfig.SetValue('EnvironmentOptions/Version/Lazarus',Version);
            if FLazarusMajorVer=-1 then
            begin // default to newest. Update this when new version appears
              NewConfig.SetValue('EnvironmentOptions/Version/Value', TrunkVersionNewEnvironmentConfig);
              NewConfig.SetValue('EnvironmentOptions/Version/Lazarus', TrunkLazarusNewEnvironmentConfig);
            end
            else if FLazarusMajorVer=0 then
            begin
              if FLazarusMinor<=0 then
                NewConfig.SetValue('EnvironmentOptions/Version/Lazarus','0.9.31');
              NewConfig.SetValue('EnvironmentOptions/Version/Value', '106')
            end
            else if FLazarusMajorVer=1 then
              case FLazarusMinor of
                0 : NewConfig.SetValue('EnvironmentOptions/Version/Value', '107'); //for version 1.0
                1 : NewConfig.SetValue('EnvironmentOptions/Version/Value', '107'); //for version 1.0,1.1
                2 : NewConfig.SetValue('EnvironmentOptions/Version/Value', '108'); //for version 1.2
                3 : NewConfig.SetValue('EnvironmentOptions/Version/Value', '108'); //for version 1.3
                4 : NewConfig.SetValue('EnvironmentOptions/Version/Value', '108'); //for version 1.4
                5 : NewConfig.SetValue('EnvironmentOptions/Version/Value', '109'); //for version 1.5
                6 : NewConfig.SetValue('EnvironmentOptions/Version/Value', '109'); //for version 1.6
                7 : NewConfig.SetValue('EnvironmentOptions/Version/Value', '110'); //for version 1.7
                8 : NewConfig.SetValue('EnvironmentOptions/Version/Value', '110'); //for version 1.8 (fixes)
                9 : NewConfig.SetValue('EnvironmentOptions/Version/Value', '110'); //for version 1.9 (old trunk)
              end
            else if FLazarusMajorVer=2 then
              case FLazarusMinor of
                0 : NewConfig.SetValue('EnvironmentOptions/Version/Value', '110'); //for version 2.0
                else
                  begin
                    NewConfig.SetValue('EnvironmentOptions/Version/Value', TrunkVersionNewEnvironmentConfig);
                    NewConfig.SetValue('EnvironmentOptions/Version/Lazarus', TrunkLazarusNewEnvironmentConfig);
                  end;
              end
            else { 3 or higher? keep latest known, we can leave lazarus version though }
            begin
              NewConfig.SetValue('EnvironmentOptions/Version/Value', TrunkVersionNewEnvironmentConfig);
            end;

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

