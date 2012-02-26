unit installerUniversal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, installerCore, m_crossinstaller,processutils;


type
  { TUniversalInstaller }

  TUniversalInstaller = class(TInstaller)
  private
    BinPath:string;
    FConfigFile:string;
    FFPCDir:string;
    FLazarusDir:string;
    InitDone:boolean;
  protected
    function GetValue(Key:string):string;
    // internal initialisation, called from BuildModule,CLeanModule,GetModule
    // and UnInstallModule but executed only once
    function InitModule:boolean;
  public
    // Configuration file in ini format containing module definitions
    property ConfigFile:string read FConfigFile write FConfigFile;
    // FPC base directory
    property FPCDir:string read FFPCDir write FFPCDir;
    // Lazarus base directory
    property LazarusDir:string read FLazarusDir write FLazarusDir;
    // Build module
    function BuildModule(ModuleName:string): boolean; override;
    // Clean up environment
    function CleanModule(ModuleName:string): boolean; override;
    // Install update sources
    function GetModule(ModuleName:string): boolean; override;
    // Gets the list of required modules for ModuleName
    function GetModuleRequirements(ModuleName:string; var RequirementList:TStringList): boolean;
    // Uninstall module
    function UnInstallModule(ModuleName:string): boolean; override;
    constructor Create;
    destructor Destroy; override;
  end;

  // Gets the list of modules enabled in ConfigFile. Appends to existing TStringList
  function GetModuleEnabledList(var ModuleList:TStringList):boolean;
  // Gets the list of modules from ConfigFile. Appends to existing TStringList
  function GetModuleList(ConfigFile:string; var ModuleList:TStringList):boolean;


var sequences:string;

implementation

uses inifiles;

Const
  STARTUSERMODULES=1000;
  MAXUSERMODULES=100;
  MAXINSTRUCTIONS=20;

var
  UniModuleList:TStringList=nil;
  UniModuleEnabledList:TStringlist=nil;

{ TUniversalInstaller }

function TUniversalInstaller.GetValue(Key: string): string;
begin

end;

function TUniversalInstaller.InitModule: boolean;
begin
  result:=true;
  if InitDone then
    exit;
  if FVerbose then
    ProcessEx.OnOutputM:=@DumpOutput;
  result:=CheckAndGetNeededExecutables;
  BinPath:=IncludeTrailingPathDelimiter(FFPCDir)+'bin'+DirectorySeparator+GetFPCTarget(true);
  InitDone:=result;
end;

function TUniversalInstaller.BuildModule(ModuleName: string): boolean;
var
  i,j,idx:integer;
  exec,cmd,param,output:string;
  Workingdir:string;
  sl:TStringList;
begin
  if not InitModule then exit;
  idx:=UniModuleList.IndexOf(ModuleName);
  if idx>=0 then
    begin
    sl:=TStringList(UniModuleList.Objects[idx]);
    Workingdir:=GetValue('Workingdir');
    for i:=1 to MAXINSTRUCTIONS do
      begin
      exec:=GetValue('InstallExecute'+IntToStr(i));
      if exec='' then break;
      //split off command and parameters
      j:=1;
      while j<=length(exec) do
        begin
        if exec[j]='"' then
          repeat  //skip until next quote
            j:=j+1;
          until (exec[j]='"') or (j=length(exec));
        j:=j+1;
        if exec[j]=' ' then break;
        end;
      cmd:=trim(copy(exec,1,j));
      param:=trim(copy(exec,j,length(exec)));
      result:=ExecuteCommandHidden(cmd,param,output,FVerbose)=0;
      if not result then
        exit;
      end;
    end
  else
    result:=false;
end;

function TUniversalInstaller.CleanModule(ModuleName: string): boolean;
begin
  if not InitModule then exit;

end;

function TUniversalInstaller.GetModule(ModuleName: string): boolean;
begin
  if not InitModule then exit;

end;

function TUniversalInstaller.GetModuleRequirements(ModuleName: string;
  var RequirementList: TStringList): boolean;
begin
  if not InitModule then exit;

end;

function TUniversalInstaller.UnInstallModule(ModuleName: string): boolean;
begin
  if not InitModule then exit;

end;

constructor TUniversalInstaller.Create;
begin
  inherited Create;
  UniModuleList:=TStringList.Create;
  UniModuleEnabledList:=TStringList.Create;
end;

destructor TUniversalInstaller.Destroy;
begin
  UniModuleList.Free;
  UniModuleEnabledList.Free;
  inherited Destroy;
end;


procedure ClearUniModuleList;
var
  i:integer;
begin
  for i:=0 to UniModuleList.Count -1 do
    FreeMem(UniModuleList.Objects[i]);
end;

procedure ReadInifile(ConfigFile: string);
var
  ini:TMemIniFile;
  i,j:integer;
  val,name:string;

  function LoadModule(ModuleName:string):boolean;
  var
    name,val:string;
    i:integer;
    sl:TStringList;
  begin
    name:=ini.ReadString(ModuleName,'Name','');
    result:=name<>'';
    if result then
      begin
      if ini.ReadString(ModuleName,'Enabled','')='1' then
        UniModuleEnabledList.Add(name);
      // store the section as is and attach as object to UniModuleList
      sl:=TstringList.Create;
      ini.ReadSection(ModuleName,sl);
      UniModuleList.AddObject(name,TObject(sl));
      end;
  end;

begin
  ini:=TMemIniFile.Create(ConfigFile);
  ini.CaseSensitive:=false;
// parse inifile
  try
    for i:=1 to STARTUSERMODULES do
      if not LoadModule('Module'+IntToStr(i)) then break; //require contiguous numbering
    for i:=STARTUSERMODULES to STARTUSERMODULES+MAXUSERMODULES do
      LoadModule('Module'+IntToStr(i));   // don't require contiguous
    // the overrides
    for i:=0 to UniModuleList.Count-1 do
      begin
      name:=UniModuleList[i];
      val:=ini.ReadString('General',name,'');
      if val='1' then
        begin //enable if not yet done
        if UniModuleEnabledList.IndexOf(name)<0 then
          UniModuleEnabledList.Add(name);
        end
      else if val='0' then
        begin //disable if enabled
        j:=UniModuleEnabledList.IndexOf(name);
        if j>=0 then
          UniModuleEnabledList.Delete(j);
        end;
      end;
  finally
    ini.Free;
  end;
end;

function GetModuleEnabledList(var ModuleList: TStringList): boolean;
var i:integer;
begin
  for i:=0 to UniModuleEnabledList.Count -1 do
    ModuleList.Add(UniModuleEnabledList[i]);
end;

function GetModuleList(ConfigFile: string; var ModuleList: TStringList
  ): boolean;
var i:integer;
begin
  ReadInifile(ConfigFile);
  for i:=0 to UniModuleList.Count -1 do
    ModuleList.Add(UniModuleList[i]);
end;

initialization
 UniModuleList:=TStringList.create;
 UniModuleEnabledList:=TStringList.create;
finalization
if assigned(UniModuleList) then
  begin
  ClearUniModuleList;
  UniModuleList.free;
  end;
if assigned(UniModuleEnabledList) then
  UniModuleEnabledList.free;
end.

