unit installerUniversal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, installerCore, m_crossinstaller,processutils,process;


type
  { TUniversalInstaller }

  TUniversalInstaller = class(TInstaller)
  private
    BinPath:string;
    FFPCDir:string;
    FLazarusDir:string;
    FLazarusPrimaryConfigPath:string;
    InitDone:boolean;
  protected
    function GetValue(Key:string;sl:TStringList):string;
    // internal initialisation, called from BuildModule,CLeanModule,GetModule
    // and UnInstallModule but executed only once
    function InitModule:boolean;
    function RunCommands(directive:string;sl:TStringList):boolean;
  public
    // FPC base directory
    property FPCDir:string read FFPCDir write FFPCDir;
    // Lazarus primary config path
    property LazarusPrimaryConfigPath:string read FLazarusPrimaryConfigPath write FLazarusPrimaryConfigPath;
    // Lazarus base directory
    property LazarusDir:string read FLazarusDir write FLazarusDir;
    // Build module
    function BuildModule(ModuleName:string): boolean; override;
    // Clean up environment
    function CleanModule(ModuleName:string): boolean; override;
    function ConfigModule(ModuleName:string): boolean; override;
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
  // Gets the sequence representation for all modules
  function GetModuleList(ConfigFile:string):string;


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

function TUniversalInstaller.GetValue(Key: string;sl:TStringList): string;
var
  i,len:integer;
  s,macro:string;
begin
  Key:=UpperCase(Key);
  s:='';
  for i:=0 to sl.Count-1 do
    begin
    s:=sl[i];
    if (copy(UpperCase(s),1, length(Key))=Key) and ((s[length(Key)+1]='=') or (s[length(Key)+1]=' ')) then
      begin
      if pos('=',s)>0 then
        s:=trim(copy(s,pos('=',s)+1,length(s)));
      break;
      end;
    s:='';
    end;
//expand macros
  if s<>'' then
    while pos('$(',s)>0 do
      begin
      i:=pos('$(',s);
      macro:=copy(s,i+2,length(s));
      if pos(')',macro)>0 then
        begin
        delete(macro,pos(')',macro),length(macro));
        macro:=UpperCase(macro);
        len:=length(macro)+3; // the brackets
        if macro='FPCDIR' then macro:=FFPCDir
        else if macro='LAZARUSDIR' then macro:=FLazarusDir
        else if macro='LAZARUSPRIMARYCONFIGPATH' then macro:=FLazarusPrimaryConfigPath
        else macro:=GetValue(macro,sl); //user defined value
        // quote if containing spaces
        if pos(' ',macro)>0 then
          macro:=''''+macro+'''';
        delete(s,i,len);
        insert(macro,s,i);
        end;
      end;
  // correct path delimiter
  for i:=1 to length(s) do
    if (s[i]='/') or (s[i]='\') then
      s[i]:=DirectorySeparator;
  result:=s;
end;

function TUniversalInstaller.InitModule: boolean;
begin
  result:=true;
  if InitDone then
    exit;
  if FVerbose then
    ProcessEx.OnOutputM:=@DumpOutput;
//  result:=CheckAndGetNeededExecutables;
  BinPath:=IncludeTrailingPathDelimiter(FFPCDir)+'bin'+DirectorySeparator+GetFPCTarget(true);
  InitDone:=result;
end;

function TUniversalInstaller.RunCommands(directive: string;sl:TStringList): boolean;
var
  i,j:integer;
  exec,output:string;
  Workingdir:string;
  PE:TProcessEx;
begin
  PE:=TProcessEx.Create(nil);
  try
    PE.CurrentDirectory:=GetValue('Workingdir',sl);
    for i:=1 to MAXINSTRUCTIONS do
      begin
      exec:=GetValue(directive+IntToStr(i),sl);
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
      PE.Executable:=trim(copy(exec,1,j));
      PE.ParametersString:=trim(copy(exec,j,length(exec)));
      PE.ShowWindow := swoHIDE;
      if FVerbose then
        PE.OnOutput:=@DumpConsole;
      PE.Execute;
      Output:=PE.OutputString;
      result:=PE.ExitStatus=0;
      if not result then
        break;
      end;
  finally
    PE.Free;
  end;
end;

function TUniversalInstaller.BuildModule(ModuleName: string): boolean;
var
  idx:integer;
  sl:TStringList;
begin
  result:=InitModule;
  if not result then exit;
  idx:=UniModuleList.IndexOf(UpperCase(ModuleName));
  if idx>=0 then
    begin
    sl:=TStringList(UniModuleList.Objects[idx]);
    WritelnLog('Building module '+ModuleName);
    result:=RunCommands('InstallExecute',sl);
    end
  else
    result:=false;
end;

function TUniversalInstaller.CleanModule(ModuleName: string): boolean;
begin
  result:=InitModule;
  if not result then exit;
  result:=true;
end;

function TUniversalInstaller.ConfigModule(ModuleName: string): boolean;
var
  idx:integer;
  sl:TStringList;

  //dummy function
  function Addxml(filename,key,value:string):boolean;
  begin
  result:=true;
  end;
  //dummy function
  function Finishxml(filename:string):boolean;
  begin
  result:=true;
  end;

  function AddToLazXML(xmlfile:string):boolean;
  var
    i,j:integer;
    exec:string;
    bdirty:boolean;
  begin
  bdirty:=false;
  for i:=1 to MAXINSTRUCTIONS do
    begin
    exec:=GetValue('AddTo'+xmlfile+IntToStr(i),sl);
    if exec='' then break;
    //split off key and value
    j:=1;
    while j<=length(exec) do
      begin
      j:=j+1;
      if exec[j]=':' then break;
      end;
    result:=Addxml(xmlfile+'.xml',trim(copy(exec,1,j-1)),trim(copy(exec,j+1,length(exec))));
    if not result then
      break;
    bdirty:=true;
    end;
  if bdirty then
    Finishxml(xmlfile+'.xml');
  end;

begin
  result:=InitModule;
  if not result then exit;
  idx:=UniModuleList.IndexOf(UpperCase(ModuleName));
  if idx>=0 then
    begin
    sl:=TStringList(UniModuleList.Objects[idx]);
    AddToLazXML('environmentoptions');
    AddToLazXML('helpoptions');
    AddToLazXML('packagefiles');
    end
  else
    result:=false;
end;

function TUniversalInstaller.GetModule(ModuleName: string): boolean;
begin
  if not InitModule then exit;
  result:=true;
end;

function TUniversalInstaller.GetModuleRequirements(ModuleName: string;
  var RequirementList: TStringList): boolean;
begin
  if not InitModule then exit;

end;

function TUniversalInstaller.UnInstallModule(ModuleName: string): boolean;
var
  idx:integer;
  sl:TStringList;
begin
  result:=InitModule;
  if not result then exit;
  idx:=UniModuleList.IndexOf(UpperCase(ModuleName));
  if idx>=0 then
    begin
    sl:=TStringList(UniModuleList.Objects[idx]);
    WritelnLog('UnInstalling module '+ModuleName);
    result:=RunCommands('UnInstallExecute',sl);
    end
  else
    result:=false;
end;

constructor TUniversalInstaller.Create;
begin
  inherited Create;
end;

destructor TUniversalInstaller.Destroy;
begin
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
        UniModuleEnabledList.Add(uppercase(name));
      // store the section as is and attach as object to UniModuleList
      sl:=TstringList.Create;
      ini.ReadSectionRaw(ModuleName,sl);
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

function GetModuleList(ConfigFile: string): string;
var
  i:integer;
  s:string;
begin
  ReadInifile(ConfigFile);
  result:='';
  for i:=0 to UniModuleList.Count -1 do
    begin
    s:=UniModuleList[i];
    result:=result+'Declare '+ s + ';' +
        'Cleanmodule '+ s +';' +
        'Getmodule '+ s +';' +
        'Buildmodule '+ s +';' +
        'Configmodule '+ s +';' +
        'End;';
    end;
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

