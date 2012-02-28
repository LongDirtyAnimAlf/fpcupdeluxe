unit installerManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,installerCore,installerFpc,installerLazarus,installerHelp,installerUniversal,fpcuputil,fileutil;

Const
  Sequences=
//default sequence
    'Declare default;'+
    'Exec CreateFpcupScript;'+
    'Do fpc;'+
    'Do lazarus;'+
    'Exec CreateLazarusScript;'+
    'Do helplazarus;'+
    'Do LAZDATADESKTOP;'+
    'Do DOCEDITOR;'+
    'End;'+
//default sequence for win32
    'Declare defaultwin32;'+
    'Exec CreateFpcupScript;'+
    'Do fpc;'+
    'Do lazarus;'+
    'Exec CreateLazarusScript;'+
    'Do helplazarus;'+
    'Do LAZDATADESKTOP;'+
    'Do DOCEDITOR;'+
    'SetCPU x86_64;'+
    'SetOS win64;'+
    'Cleanmodule fpc;'+
    'Buildmodule fpc;'+
    'Cleanmodule lazarus;'+
    'Buildmodule lazarus;'+
    'End;'+
//default clean sequence
    'Declare defaultclean;'+
    'Do fpcclean;'+
    'Do lazarusclean;'+
    'Do helplazarusclean;'+
    'CleanModule LAZDATADESKTOP;'+
    'CleanModule DOCEDITOR;'+
    'End;'+
//default clean sequence for win32
    'Declare defaultwin32clean;'+
    'Do fpcclean;'+
    'Do lazarusclean;'+
    'Do helplazarusclean;'+
    'CleanModule LAZDATADESKTOP;'+
    'CleanModule DOCEDITOR;'+
    'SetCPU x86_64;'+
    'SetOS win64;'+
    'Cleanmodule fpc;'+
    'Cleanmodule lazarus;'+
    'End;'+

//default uninstall sequence
    'Declare defaultuninstall;'+
    'Do fpcuninstall;'+
    'Do lazarusuninstall;'+
    'Do helpuninstall;'+
    'UninstallModule LAZDATADESKTOP;'+
    'UninstallModule DOCEDITOR;'+
    'End;'+
//default clean sequence for win32
    'Declare defaultwin32clean;'+
    'Do defaultuninstall;'+
    'End;';

type
  TSequencer=class; //forward


  { TFPCupManager }

  TFPCupManager=class(Tobject)
  private
    FAllOptions: string;
    FBootstrapCompiler: string;
    FBootstrapCompilerDirectory: string;
    FBootstrapCompilerURL: string;
    FClean: boolean;
    FCompilerName: string;
    FConfigFile: string;
    FCrossCPU_Target: string;
    FCrossLCL_Platform: string;
    FCrossOS_Target: string;
    FFPCDesiredRevision: string;
    FFPCDirectory: string;
    FFPCOPT: string;
    FFPCURL: string;
    FLazarusDesiredRevision: string;
    FLazarusDirectory: string;
    FLazarusOPT: string;
    FLazarusPrimaryConfigPath: string;
    FLazarusURL: string;
    FMakeDirectory: string;
    FOnlyModules: string;
    FShortCutName: string;
    FShortCutNameFpcup: string;
    FSkipModules: string;
    FUninstall:boolean;
    FVerbose: boolean;
    Sequencer: TSequencer;
    function GetLazarusPrimaryConfigPath: string;
    procedure SetBootstrapCompilerDirectory(AValue: string);
    procedure SetFPCDirectory(AValue: string);
    procedure SetLazarusDirectory(AValue: string);
    procedure SetMakeDirectory(AValue: string);
  protected
    LogFile:Text;
    VerBoseLog:Text;
    FModuleList:TStringList;
    FModuleEnabledList:TStringList;
    FModulePublishedList:TStringList;
    // write verbatim to log and eventually console
    procedure WriteLog(msg:string;ToConsole:boolean=true);
    // append line ending and write to log and eventually console
    procedure WritelnLog(msg:string;ToConsole:boolean=true);
 public
    property ShortCutName: string read FShortCutName write FShortCutName;
    property ShortCutNameFpcup:string read FShortCutNameFpcup write FShortCutNameFpcup;
    property CompilerName: string read FCompilerName write FCompilerName;
    property AllOptions:string read FAllOptions write FAllOptions;
    property BootstrapCompiler: string read FBootstrapCompiler write FBootstrapCompiler;
    property BootstrapCompilerDirectory: string read FBootstrapCompilerDirectory write SetBootstrapCompilerDirectory;
    property BootstrapCompilerURL: string read FBootstrapCompilerURL write FBootstrapCompilerURL;
    property Clean: boolean read FClean write FClean;
    property ConfigFile: string read FConfigFile write FConfigFile;
    property CrossCPU_Target:string read FCrossCPU_Target write FCrossCPU_Target;
    property CrossLCL_Platform:string read FCrossLCL_Platform write FCrossLCL_Platform;
    property CrossOS_Target:string read FCrossOS_Target write FCrossOS_Target;
    property FPCDirectory: string read FFPCDirectory write SetFPCDirectory;
    property FPCURL: string read FFPCURL write FFPCURL;
    property FPCOPT: string read FFPCOPT write FFPCOPT;
    property FPCDesiredRevision:string read FFPCDesiredRevision write FFPCDesiredRevision;
    property LazarusDirectory: string read FLazarusDirectory write SetLazarusDirectory;
    property LazarusPrimaryConfigPath: string read GetLazarusPrimaryConfigPath write FLazarusPrimaryConfigPath ;
    property LazarusURL: string read FLazarusURL write FLazarusURL;
    property LazarusOPT:string read FLazarusOPT write FLazarusOPT;
    property LazarusDesiredRevision:string read FLazarusDesiredRevision write FLazarusDesiredRevision;
    property MakeDirectory: string read FMakeDirectory write SetMakeDirectory;
    //List of all default enabled sequences available
    property ModuleEnabledList: TStringList read FModuleEnabledList;
    //List of all publicly visible sequences
    property ModulePublishedList: TStringList read FModulePublishedList;
    property SkipModules:string read FSkipModules write FSkipModules;
    property OnlyModules:string read FOnlyModules write FOnlyModules;
    property Uninstall: boolean read FUninstall write FUninstall;
    property Verbose:boolean read FVerbose write FVerbose;
    // Fill in ModulePublishedList and ModuleEnabledList
    function LoadModuleList:boolean;
    // Stop talking. Do it!
    function Run: boolean;
    constructor Create;
    destructor Destroy; override;
  end;


  TExecState=(ESNever,ESFailed,ESSucceeded);

  TSequenceAttributes=record
    EntryPoint:integer;  //instead of rescanning the sequence table everytime, we can as well store the index in the table
    Executed:TExecState; //  Reset to ESNever at sequencer start up
  end;
  PSequenceAttributes=^TSequenceAttributes;

  TKeyword=(SMdeclare, SMdeclareHidden, SMdo, SMrequire, SMexec, SMend, SMcleanmodule, SMgetmodule, SMbuildmodule,
    SMuninstallmodule, SMconfigmodule, SMSetLCL, SMSetOS, SMSetCPU,SMInvalid);

  TState=record
    instr:TKeyword;
    param:string;
    end;

  { TSequencer }

  TSequencer=class(TObject)
    protected
      FParent:TFPCupManager;
      CurrentModule:String;
      Installer:TInstaller;  //current installer
      SkipList:TStringList;
      StateMachine:array of TState;
      procedure AddToModuleList(ModuleName:string;EntryPoint:integer);
      function DoBuildModule(ModuleName:string):boolean;
      function DoCleanModule(ModuleName:string):boolean;
      function DoConfigModule(ModuleName:string):boolean;
      function DoExec(FunctionName:string):boolean;
      function DoGetModule(ModuleName:string):boolean;
      function DoSetCPU(CPU:string):boolean;
      function DoSetOS(OS:string):boolean;
      function DoSetLCL(LCL:string):boolean;
      function DoUnInstallModule(ModuleName:string):boolean;
      function GetInstaller(ModuleName:string):boolean;
      function IsSkipped(ModuleName:string):boolean;
      procedure ResetAllExecuted(SkipFPC:boolean=false);
    public
      property Parent:TFPCupManager write Fparent;
      // parse a sequence source code and append to the statemachine
      function AddSequence(Sequence:string):boolean;
      // Add the "only" sequence from the statemachine based on the --only list
      function CreateOnly(OnlyModules:string):boolean;
      // deletes the "only" sequence from the statemachine
      function DeleteOnly:boolean;
      // run the statemachine starting at SequenceName
      function Run(SequenceName:string):boolean;
      constructor Create;
      destructor Destroy; override;
    end;

implementation

{ TFPCupManager }

function TFPCupManager.GetLazarusPrimaryConfigPath: string;
const
  DefaultPCPSubdir='lazarusdevsettings'; //Include the name lazarus for easy searching Caution: shouldn't be the same name as Lazarus dir itself.
begin
  if FLazarusPrimaryConfigPath='' then
    begin
      {$IFDEF MSWINDOWS}
      // Somewhere in local appdata special folder
      FLazarusPrimaryConfigPath := IncludeTrailingPathDelimiter(GetLocalAppDataPath())+DefaultPCPSubdir;
      {$ELSE}
      //Note: normsl GetAppConfigDir gets ~/.config/fpcup/.lazarusdev or something
      LazarusPrimaryConfigPath:=IncludeTrailingPathDelimiter(XdgConfigHome)+DefaultPCPSubdir;
      {$ENDIF MSWINDOWS}
    end;
  result:=FLazarusPrimaryConfigPath;
end;

procedure TFPCupManager.SetBootstrapCompilerDirectory(AValue: string);
begin
FBootstrapCompilerDirectory:=IncludeTrailingPathDelimiter(ExpandFileName(AValue));
end;

procedure TFPCupManager.SetFPCDirectory(AValue: string);
begin
  FFPCDirectory:=ExpandFileName(AValue);
end;

procedure TFPCupManager.SetLazarusDirectory(AValue: string);
begin
  FLazarusDirectory:=ExpandFileName(AValue);
end;

procedure TFPCupManager.SetMakeDirectory(AValue: string);
begin
  FMakeDirectory:=ExpandFileName(AValue);
end;

procedure TFPCupManager.WriteLog(msg: string; ToConsole: boolean);
begin
Write(LogFile,msg);
if ToConsole then
  InfoLn(msg);
end;

procedure TFPCupManager.WritelnLog(msg: string; ToConsole: boolean);
begin
WriteLog(msg+LineEnding,false); //infoln adds already a lf
if ToConsole then
  InfoLn(msg);
end;

function TFPCupManager.LoadModuleList: boolean;
var i:integer;
begin
Sequencer.AddSequence(Sequences);
Sequencer.AddSequence(installerFPC.Sequences);
Sequencer.AddSequence(installerLazarus.Sequences);
Sequencer.AddSequence(installerHelp.Sequences);
Sequencer.AddSequence(installerUniversal.Sequences);
//append universal modules to the lists
Sequencer.AddSequence(installerUniversal.GetModuleList(FConfigFile));
installerUniversal.GetModuleEnabledList(FModuleEnabledList);
end;

function TFPCupManager.Run: boolean;

begin
  result:=false;
  if FOnlyModules<>'' then
    begin
    Sequencer.CreateOnly(FOnlyModules);
    result:=Sequencer.Run('Only');
    Sequencer.DeleteOnly;
    end
  else
    {$ifdef win32}
    if pos('WINCROSSX64',UpperCase(SkipModules))>0 then
      result:=Sequencer.Run('Default')
    else
      result:=Sequencer.Run('DefaultWin32');
    {$else}
    result:=Sequencer.Run('Default');
    {$endif win32}
end;

constructor TFPCupManager.Create;
var
  LogFileName: string;
begin
FModuleList:=TStringList.Create;
FModuleEnabledList:=TStringList.Create;
FModulePublishedList:=TStringList.Create;
Sequencer:=TSequencer.create;
Sequencer.Parent:=Self;
{$IFDEF MSWINDOWS}
LogFileName:='fpcup.log'; //current directory
{$ELSE}
LogFileName:=ExpandFileNameUTF8('~')+DirectorySeparator+'fpcup.log'; //In home directory
{$ENDIF MSWINDOWS}
try
 AssignFile(LogFile,LogFileName);
 if FileExistsUTF8(LogFileName) then
   Append(LogFile)
 else
   Rewrite(LogFile);
except
  infoln('Error: could not open log file '+LogFileName+' for writing.');
  infoln('This may be caused by another fpcup currently running.');
  infoln('Aborting.');
  halt(2); //Is there a nicer way to do this?
end;
WritelnLog(DateTimeToStr(now)+': fpcup started.',false);
end;

destructor TFPCupManager.Destroy;
var i:integer;
begin
  for i:=0 to FModuleList.Count-1 do
    Freemem(FModuleList.Objects[i]);
  FModuleList.Free;
  FModulePublishedList.Free;
  FModuleEnabledList.Free;
  Sequencer.free;
  WritelnLog(DateTimeToStr(now)+': fpcup finished.',false);
  WritelnLog('------------------------------------------------',false);
  CloseFile(LogFile);
  inherited Destroy;
end;

{ TSequencer }

procedure TSequencer.AddToModuleList(ModuleName: string; EntryPoint: integer);
var
  SeqAttr:PSequenceAttributes;
begin
getmem(SeqAttr,sizeof(TSequenceAttributes));
SeqAttr^.EntryPoint:=EntryPoint;
SeqAttr^.Executed:=ESNever;
FParent.FModuleList.AddObject(ModuleName,TObject(SeqAttr));
end;

function TSequencer.DoBuildModule(ModuleName: string): boolean;
begin
  result:= GetInstaller(ModuleName) and Installer.BuildModule(ModuleName);
end;

function TSequencer.DoCleanModule(ModuleName: string): boolean;
begin
  result:= GetInstaller(ModuleName) and Installer.CleanModule(ModuleName);
end;

function TSequencer.DoConfigModule(ModuleName: string): boolean;
begin
  result:= GetInstaller(ModuleName) and Installer.ConfigModule(ModuleName);
end;

function TSequencer.DoExec(FunctionName: string): boolean;

  function CreateFpcupScript:boolean;
  begin
    result:=true;
    // Link to fpcup itself, with all options as passed when invoking it:
    if FParent.ShortCutNameFpcup<>EmptyStr then
    begin
     {$IFDEF MSWINDOWS}
      CreateDesktopShortCut(paramstr(0),FParent.AllOptions,FParent.ShortCutNameFpcup);
     {$ELSE}
      FParent.AllOptions:=FParent.AllOptions+' $*';
      CreateHomeStartLink('"'+paramstr(0)+'"',FParent.AllOptions,FParent.ShortCutNameFpcup);
     {$ENDIF MSWINDOWS}
    end;
  end;


  function CreateLazarusScript:boolean;
  //calculate InstalledLazarus. Don't use this function when lazarus is not installed.
  var
    InstalledLazarus:string;
  begin
  result:=true;
  if FParent.ShortCutName<>EmptyStr then
  begin
    infoln('Lazarus: creating desktop shortcut:');
    try
      //Create shortcut; we don't care very much if it fails=>don't mess with OperationSucceeded

      InstalledLazarus:=IncludeTrailingPathDelimiter(FParent.LazarusDirectory)+'lazarus'+GetExeExt;
      {$IFDEF MSWINDOWS}
      CreateDesktopShortCut(InstalledLazarus,'--pcp="'+FParent.LazarusPrimaryConfigPath+'"',FParent.ShortCutName);
      {$ENDIF MSWINDOWS}
      {$IFDEF UNIX}
      CreateHomeStartLink(InstalledLazarus,'--pcp="'+FParent.LazarusPrimaryConfigPath+'"',FParent.ShortcutName);
      {$ENDIF UNIX}
    finally
      //Ignore problems creating shortcut
    end;
  end;
  end;

  function DeleteLazarusScript:boolean;
  //calculate InstalledLazarus. Don't use this function when lazarus is not installed.
  var
    InstalledLazarus:string;
  begin
  result:=true;
  if FParent.ShortCutName<>EmptyStr then
  begin
    infoln('Lazarus: deleting desktop shortcut:');
    try
      //Create shortcut; we don't care very much if it fails=>don't mess with OperationSucceeded

      InstalledLazarus:=IncludeTrailingPathDelimiter(FParent.LazarusDirectory)+'lazarus'+GetExeExt;
      {$IFDEF MSWINDOWS}
//todo      DeleteDesktopShortCut(InstalledLazarus,'--pcp="'+FParent.LazarusPrimaryConfigPath+'"',FParent.ShortCutName);
      {$ENDIF MSWINDOWS}
      {$IFDEF UNIX}
      DeleteFileUTF8(FParent.ShortcutName);
      {$ENDIF UNIX}
    finally
      //Ignore problems creating shortcut
    end;
  end;
  end;

begin
  if UpperCase(FunctionName)='CREATEFPCUPSCRIPT' then
    result:=CreateFpcupScript
  else if UpperCase(FunctionName)='CREATELAZARUSSCRIPT' then
    result:=CreateLazarusScript
  else if UpperCase(FunctionName)='DELETELAZARUSSCRIPT' then
    result:=DeleteLazarusScript
  else
    begin
    result:=false;
    FParent.WritelnLog('Error: Trying to execute a non existing function: ' + FunctionName);
    end;
end;

function TSequencer.DoGetModule(ModuleName: string): boolean;
begin
  result:= GetInstaller(ModuleName) and Installer.GetModule(ModuleName);
end;

function TSequencer.DoSetCPU(CPU: string): boolean;
begin
  FParent.CrossCPU_Target:=CPU;
  ResetAllExecuted;
end;

function TSequencer.DoSetOS(OS: string): boolean;
begin
  FParent.CrossOS_Target:=OS;
  ResetAllExecuted;
end;

function TSequencer.DoSetLCL(LCL: string): boolean;
begin
  FParent.CrossLCL_Platform:=LCL;
  ResetAllExecuted(true);
end;

function TSequencer.DoUnInstallModule(ModuleName: string): boolean;
begin
  result:= GetInstaller(ModuleName) and Installer.UnInstallModule(ModuleName);
end;

{GetInstaller gets a new installer for ModuleName and initialises parameters unless one exist already.}

function TSequencer.GetInstaller(ModuleName: string): boolean;
var
  CrossCompiling:boolean;
begin
  CrossCompiling:= (FParent.CrossCPU_Target<>'') or (FParent.CrossOS_Target<>'');
  //check if this is a known module
  if uppercase(ModuleName)='FPC' then
    begin
    if assigned(Installer) then
      begin
      if (not crosscompiling and (Installer is TFPCNativeInstaller)) or
        ( crosscompiling and (Installer is TFPCCrossInstaller)) then
        begin
        result:=true; //all fine, continue with current installer
        exit;
        end
      else
        Installer.free; // get rid of old installer
      end;
    if CrossCompiling then
      begin
      Installer:=TFPCCrossInstaller.Create;
      Installer.CrossOS_Target:=FParent.CrossOS_Target;
      Installer.CrossCPU_Target:=FParent.CrossCPU_Target;
      end
    else
      Installer:=TFPCNativeInstaller.Create;
    Installer.BaseDirectory:=FParent.FPCDirectory;
    (Installer as TFPCInstaller).BootstrapCompilerDirectory:=FParent.BootstrapCompilerDirectory;
    (Installer as TFPCInstaller).BootstrapCompilerURL:=FParent.BootstrapCompilerURL;
    Installer.Compiler:='';  //bootstrap used
    Installer.CompilerOptions:=FParent.FPCOPT;
    Installer.DesiredRevision:=FParent.FPCDesiredRevision;
    Installer.LogFile:=FParent.LogFile;
    {$IFDEF MSWINDOWS}
    Installer.MakeDirectory:=FParent.MakeDirectory;
    {$ENDIF}
    Installer.URL:=FParent.FPCURL;
    Installer.Verbose:=FParent.Verbose;
    end
  else if (uppercase(ModuleName)='LAZARUS') or (uppercase(ModuleName)='BIGIDE') then
    begin
    if assigned(Installer) then
      begin
      if (not crosscompiling and (Installer is TLazarusNativeInstaller)) or
        ( crosscompiling and (Installer is TLazarusCrossInstaller)) then
        begin
        result:=true; //all fine, continue with current installer
        exit;
        end
      else
        Installer.free; // get rid of old installer
      end;
    if CrossCompiling then
      begin
      Installer:=TLazarusCrossInstaller.Create;
      Installer.CrossOS_Target:=FParent.CrossOS_Target;
      Installer.CrossCPU_Target:=FParent.CrossCPU_Target;
      end
    else
      Installer:=TLazarusNativeInstaller.Create;
    Installer.BaseDirectory:=FParent.LazarusDirectory ;
    if FParent.CompilerName='' then
      Installer.Compiler:=Installer.GetCompilerInDir(FParent.FPCDirectory)
    else
      Installer.Compiler:=FParent.CompilerName;
    Installer.CompilerOptions:=FParent.LazarusOPT;
    Installer.DesiredRevision:=FParent.LazarusDesiredRevision;
    (Installer As TLazarusInstaller).FPCDir:=FParent.FPCDirectory;
    Installer.LogFile:=FParent.LogFile;
    {$IFDEF MSWINDOWS}
    Installer.MakeDirectory:=FParent.MakeDirectory;
    {$ENDIF}
    (Installer as TLazarusInstaller).PrimaryConfigPath:=FParent.LazarusPrimaryConfigPath;
    Installer.URL:=FParent.FPCURL;
    Installer.Verbose:=FParent.Verbose;
    end
  //Convention: help modules start with HelpFPC
  //or HelpLazarus
  else if uppercase(ModuleName)='HELPFPC' then
      begin
      if assigned(Installer) then
        begin
        if (Installer is THelpFPCInstaller) then
          begin
          result:=true; //all fine, continue with current installer
          exit;
          end
        else
          Installer.free; // get rid of old installer
        end;
      Installer:=THelpFPCInstaller.Create;
      Installer.BaseDirectory:=FParent.FPCDirectory;
      if FParent.CompilerName='' then
        Installer.Compiler:=Installer.GetCompilerInDir(FParent.FPCDirectory)
      else
        Installer.Compiler:=FParent.CompilerName;
      Installer.LogFile:=FParent.LogFile;
      {$IFDEF MSWINDOWS}
      Installer.MakeDirectory:=FParent.MakeDirectory;
      {$ENDIF}
      Installer.Verbose:=FParent.Verbose;
      end
  else if uppercase(ModuleName)='HELPLAZARUS' then
      begin
      if assigned(Installer) then
        begin
       if (Installer is THelpLazarusInstaller) then
          begin
          result:=true; //all fine, continue with current installer
          exit;
          end
        else
          Installer.free; // get rid of old installer
        end;
      Installer:=THelpLazarusInstaller.Create;
      Installer.BaseDirectory:=FParent.LazarusDirectory ;
      if FParent.CompilerName='' then
        Installer.Compiler:=Installer.GetCompilerInDir(FParent.FPCDirectory)
      else
        Installer.Compiler:=FParent.CompilerName;
      (Installer as THelpLazarusInstaller).FPCDirectory:=FParent.FPCDirectory;
      Installer.LogFile:=FParent.LogFile;
      {$IFDEF MSWINDOWS}
      Installer.MakeDirectory:=FParent.MakeDirectory;
      {$ENDIF}
      (Installer as THelpLazarusInstaller).LazarusPrimaryConfigPath:=FParent.LazarusPrimaryConfigPath;
      Installer.Verbose:=FParent.Verbose;
      end
  else       // this is a universal module
    begin
      if assigned(Installer) then
        begin
        if (Installer is TUniversalInstaller) and
          (CurrentModule= ModuleName) then
          begin
          result:=true; //all fine, continue with current installer
          exit;
          end
        else
          Installer.free; // get rid of old installer
        end;
      Installer:=TUniversalInstaller.Create;
      CurrentModule:=ModuleName;
      //assign properties
      (Installer as TUniversalInstaller).FPCDir:=FParent.FFPCDirectory;
      (Installer as TUniversalInstaller).LazarusDir:=FParent.FLazarusDirectory;
      (Installer as TUniversalInstaller).LazarusPrimaryConfigPath:=FParent.LazarusPrimaryConfigPath;
      Installer.LogFile:=FParent.LogFile;
      {$IFDEF MSWINDOWS}
      Installer.MakeDirectory:=FParent.MakeDirectory;
      {$ENDIF}
      if FParent.CompilerName='' then
        Installer.Compiler:=Installer.GetCompilerInDir(FParent.FPCDirectory)
      else
        Installer.Compiler:=FParent.CompilerName;
      Installer.Verbose:=FParent.Verbose;
    end;
end;

function TSequencer.IsSkipped(ModuleName: string): boolean;
begin
  result:=assigned(SkipList) and (SkipList.IndexOf(Uppercase(ModuleName))>=0);
end;

procedure TSequencer.ResetAllExecuted(SkipFPC: boolean);
var
  idx:integer;
begin
for idx:=0 to FParent.FModuleList.Count -1 do
  // convention: FPC sequences that are to be skipped start with 'FPC'. Used in SetLCL.
  // todo: skip also help???? Who would call several help installs in one sequence? SubSequences?
  if not SkipFPC or (pos('FPC',Uppercase(FParent.FModuleList[idx]))<>1) then
    PSequenceAttributes(FParent.FModuleList.Objects[idx])^.Executed:=ESNever;
end;

function TSequencer.AddSequence(Sequence: string): boolean;
//our mini parser
var
  line,key,param:string;
  i:integer;
  instr:TKeyword;
  sequencename:string;

  function KeyStringToKeyword(Key:string):TKeyword;

  begin
    if key='DECLARE' then result:=SMdeclare
    else if key='DECLAREHIDDEN' then result:=SMdeclareHidden
    else if key='DO' then result:=SMdo
    else if key='REQUIRES' then result:=SMrequire
    else if key='EXEC' then result:=SMexec
    else if key='END' then result:=SMend
    else if key='CLEANMODULE' then result:=SMcleanmodule
    else if key='GETMODULE' then result:=SMgetmodule
    else if key='BUILDMODULE' then result:=SMbuildmodule
    else if key='UNINSTALLMODULE' then result:=SMuninstallmodule
    else if key='CONFIGMODULE' then result:=SMconfigmodule
    else if key='SETLCL' then result:=SMSetLCL
    else if key='SETOS' then result:=SMSetOS
    else if key='SETCPU' then result:=SMSetCPU
    else result:=SMInvalid;
  end;

  //remove white space and line terminator
  function NoWhite(s:string):string;
  begin
    while (s[1]<=' ') or (s[1]=';') do delete(s,1,1);
    while (s[length(s)]<=' ') or (s[length(s)]=';') do delete(s,length(s),1);
    result:=s;
  end;

begin
while Sequence<>'' do
  begin
  i:=pos(';',Sequence);
  if i>0 then
    line:=copy(Sequence,1,i-1)
  else
    line:=Sequence;
  delete(Sequence,1,length(line)+1);
  line:=NoWhite(line);
  if line<>'' then
    begin
    i:=pos(' ',line);
    if i>0 then
      begin
      key:=copy(line,1,i-1);
      param:=NoWhite(copy(line,i,length(line)));
      end
    else
      begin
      key:=line;
      param:='';
      end;
    key:=NoWhite(key);
    if key<>'' then
      begin
      i:=Length(StateMachine);
      SetLength(StateMachine,i+1);
      instr:=KeyStringToKeyword(Uppercase(Key));
      StateMachine[i].instr:=instr;
      if instr=SMInvalid then
        FParent.WritelnLog('Invalid instruction '+Key+' in sequence '+sequencename);
      StateMachine[i].param:=param;
      if instr in [SMdeclare,SMdeclareHidden] then
        begin
        AddToModuleList(uppercase(param),i);
        sequencename:=param;
        end;
      if instr = SMdeclare then
        FParent.FModulePublishedList.Add(param);
      end;
    end;
  end;
result:=true;
end;


// create the sequence corresponding with the only parameters
function TSequencer.CreateOnly(OnlyModules: string): boolean;
var
  i:integer;
  seq:string;

begin
AddToModuleList('ONLY',Length(StateMachine));
while Onlymodules<>'' do
  begin
  i:=pos(',',Onlymodules);
  if i>0 then
    seq:=copy(Onlymodules,1,i-1)
  else
    seq:=Onlymodules;
  delete(Onlymodules,1,length(seq)+1);
  // We could build a sequence string and have it parsed by AddSequence.
  // Pro: no dependency on statemachine structure
  // Con: dependency on sequence format; double parsing
  if seq<>'' then
    begin
    i:=Length(StateMachine);
    SetLength(StateMachine,i+1);
    StateMachine[i].instr:=SMdo;
    StateMachine[i].param:=seq;
    end;
  end;
i:=Length(StateMachine);
SetLength(StateMachine,i+1);
StateMachine[i].instr:=SMend;
StateMachine[i].param:='';
result:=true;
end;

function TSequencer.DeleteOnly: boolean;
var i,idx:integer;
  SeqAttr:^TSequenceAttributes;
begin
idx:=FParent.FModuleList.IndexOf('ONLY');
if (idx >0) then
  begin
  SeqAttr:=PSequenceAttributes(pointer(FParent.FModuleList.Objects[idx]));
  i:=SeqAttr^.EntryPoint;
  while i<length(StateMachine) do
    begin
    StateMachine[i].param:='';
    i:=i+1;
    end;
  SetLength(StateMachine,SeqAttr^.EntryPoint);
  Freemem(FParent.FModuleList.Objects[idx]);
  FParent.FModuleList.Delete(idx);
  end;
end;

function TSequencer.Run(SequenceName: string): boolean;
var
  EntryPoint,InstructionPointer:integer;
  idx:integer;
  SeqAttr:^TSequenceAttributes;

  Procedure CleanUpInstaller;
  begin
    if assigned(Installer) then
      begin
      Installer.Free;
      Installer:=nil;
      end;
  end;

begin
  if not assigned(FParent.FModuleList) then
    begin
    result:=false;
    FParent.WritelnLog('Error: No sequences loaded when trying to find' + SequenceName);
    exit;
    end;
  // --clean or --install ??
  if FParent.Uninstall then  // uninstall overrides clean
    begin
    if (UpperCase(SequenceName)<>'ONLY') and (uppercase(copy(SequenceName,length(SequenceName)-8,9))<>'UNINSTALL') then
      SequenceName:=SequenceName+'uninstall';
    end
  else if FParent.Clean  then
    begin
    if (UpperCase(SequenceName)<>'ONLY') and (uppercase(copy(SequenceName,length(SequenceName)-4,5))<>'CLEAN') then
      SequenceName:=SequenceName+'clean';
    end;
  // find sequence
  idx:=FParent.FModuleList.IndexOf(Uppercase(SequenceName));
  if (idx>=0) then
    begin
    result:=true;
    SeqAttr:=PSequenceAttributes(pointer(FParent.FModuleList.Objects[idx]));
    // Don't run if already run
    case SeqAttr^.Executed of
      ESFailed   : begin
                     result:=false;
                     exit;
                   end;
      ESSucceeded : exit;
      end;
    // Get entry point in statemachine
    InstructionPointer:=SeqAttr^.EntryPoint;
    EntryPoint:=InstructionPointer;
    // run sequence until end or failure
    while true do
      begin
      case StateMachine[InstructionPointer].instr of
        SMdeclare     :;
        SMdeclareHidden :;
        SMdo          : if not IsSkipped(StateMachine[InstructionPointer].param) then
                          result:=Run(StateMachine[InstructionPointer].param);
        SMrequire     : result:=Run(StateMachine[InstructionPointer].param);
        SMexec        : result:=DoExec(StateMachine[InstructionPointer].param);
        SMend         : begin
                          SeqAttr^.Executed:=ESSucceeded;
                          CleanUpInstaller;
                          exit; //success
                        end;
        SMcleanmodule : result:=DoCleanModule(StateMachine[InstructionPointer].param);
        SMgetmodule   : result:=DoGetModule(StateMachine[InstructionPointer].param);
        SMbuildmodule : result:=DoBuildModule(StateMachine[InstructionPointer].param);
        SMuninstallmodule: result:=DoUnInstallModule(StateMachine[InstructionPointer].param);
        SMconfigmodule: result:=DoConfigModule(StateMachine[InstructionPointer].param);
        SMSetLCL      : DoSetLCL(StateMachine[InstructionPointer].param);
        SMSetOS       : DoSetOS(StateMachine[InstructionPointer].param);
        SMSetCPU      : DoSetCPU(StateMachine[InstructionPointer].param);
        end;
      if not result then
        begin
        SeqAttr^.Executed:=ESFailed;
        FParent.WritelnLog('Error running fpcup. Technical details: error executing sequence '+SequenceName+
          '; line: '+IntTostr(InstructionPointer - EntryPoint+1)+
          ', param: '+StateMachine[InstructionPointer].param);
        CleanUpInstaller;
        exit; //failure, bail out
        end;
      InstructionPointer:=InstructionPointer+1;
      if InstructionPointer>=length(StateMachine) then  //somebody forgot end
        begin
        SeqAttr^.Executed:=ESSucceeded;
        CleanUpInstaller;
        exit; //success
        end;
      end;
    end
  else
    begin
    result:=false;  // sequence not found
    FParent.WritelnLog('Error: Failed to load sequence :' + SequenceName);
    end;
end;

constructor TSequencer.Create;
begin

end;

destructor TSequencer.Destroy;
begin
  inherited Destroy;
end;

end.

