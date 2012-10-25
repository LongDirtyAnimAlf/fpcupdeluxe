unit installerManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,installerCore,installerFpc,installerLazarus,installerHelp,installerUniversal,fpcuputil,fileutil
  {$ifdef linux}
  ,dynlibs
  {$endif linux}
  ;

// Get revision from our source code repository:
// If you have a file not found error for revision.inc, please make sure you compile hgversion.pas before compiling this project.
{$i revision.inc}
//Contains RevisionStr and versiondate constants

// These sequences determine standard installation/uninstallation order/content:
Const
  Sequences=
//default sequence. Using declare makes this show up in the module list given by fpcup --help
    // If you don't want that, use DeclareHidden
    'Declare default;'+
    {$ifdef linux}
    'Exec CheckDevLibs;'+
    {$endif linux}
    'Do fpc;'+
    //Get bigide so we at least have a compiler:
    'Do BIGIDE;'+
    'Do helplazarus;'+
    'Do LAZDATADESKTOP;'+
    'Do DOCEDITOR;'+
    //Get external packages/universal modules
    'Do UniversalDefault;'+
    //Recompile user IDE so any packages selected by the
    //universal installer are compiled into the IDE:
    'Do USERIDE;'+
    'End;'+
//default sequence for win32
    'Declare defaultwin32;'+
    'Do fpc;'+
    //Get bigide so we at least have a working IDE:
    'Do BIGIDE;'+
    'Do helplazarus;'+
    'Do LAZDATADESKTOP;'+
    'Do DOCEDITOR;'+
    //Get external packages/universal modules
    'Do UniversalDefault;'+
    //Recompile user IDE so any packages selected by the
    //universal installer are compiled into the IDE:
    'Do USERIDE;'+
    'Do crosswin32-64;'+  //this has to be the last. All TExecState reset!
    'End;'+
//cross sequence for win32. Note: if changing this name,
    //also change checks for this in skipmodules etc.
    'Declare crosswin32-64;'+
    'SetCPU x86_64;'+
    'SetOS win64;'+
    'Cleanmodule fpc;'+
    // Getmodule has already been done; however nuclear cleaning removes a lot of
    // files that should still be there. Workaround until we do an svn up in the cleaner module
    'Getmodule fpc;'+
    'Buildmodule fpc;'+
    // Getmodule has already been donecleaner module
    'Cleanmodule LCL;'+
    'Buildmodule LCL;'+
    'End;'+
//default sequence for win64
{todo: win64 sequence currently not enabled; see
{$elseif defined(win64)}
below}
//todo: if enabled, check if workaround for nuclear cleaning (see above) is also needed
    'Declare defaultwin64;'+
    'Do fpc;'+
    //Get bigide so we at least have a compiler:
    'Do BIGIDE;'+
    'Do helplazarus;'+
    'Do LAZDATADESKTOP;'+
    'Do DOCEDITOR;'+
    //Get external packages/universal modules
    'Do UniversalDefault;'+
    //Recompile user IDE so any packages selected by the
    //universal installer are compiled into the IDE:
    'Do USERIDE;'+
    'Do crosswin64-32;'+  //this has to be the last. All TExecState reset!
    'End;'+
//cross sequence for win32. Note: if changing this name,
    //also change checks for this in skipmodules etc.
    'Declare crosswin64-32;'+
    'SetCPU i386;'+
    'SetOS win32;'+
    'Cleanmodule fpc;'+
    'Buildmodule fpc;'+
    //Getmodule has already been done
    'Cleanmodule LCL;'+
    'Buildmodule LCL;'+
    'End;'+
//default clean sequence
    'Declare defaultclean;'+
    'Do fpcclean;'+
    'Do lazarusclean;'+
    'Do helplazarusclean;'+
    'CleanModule LAZDATADESKTOP;'+
    'CleanModule DOCEDITOR;'+
    'Do UniversalDefaultClean;'+
    'End;'+
//default clean sequence for win32
    'Declare defaultwin32clean;'+
    'Do fpcclean;'+
    'Do lazarusclean;'+
    'Do helplazarusclean;'+
    'CleanModule LAZDATADESKTOP;'+
    'CleanModule DOCEDITOR;'+
    'Do UniversalDefaultClean;'+
    'Do crosswin32-64Clean;'+   //this has to be the last. All TExecState reset!
    'End;'+
//default cross clean sequence for win32
    'Declare crosswin32-64Clean;'+
    'SetCPU x86_64;'+
    'SetOS win64;'+
    'Cleanmodule fpc;'+
    'Cleanmodule lazarus;'+
    'End;'+
//default cross clean sequence for win64
    'Declare crosswin64-32Clean;'+
    'SetCPU i386;'+
    'SetOS win32;'+
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
    'Do UniversalDefaultUnInstall;'+
    'End;'+
//default uninstall sequence for win32
    'Declare defaultwin32uninstall;'+
    'Do defaultuninstall;'+
    'End;'+
//default uninstall sequence for win64
    'Declare defaultwin64uninstall;'+
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
    FKeepLocalDiffs: boolean;
    FLazarusDesiredRevision: string;
    FLazarusDirectory: string;
    FLazarusOPT: string;
    FLazarusPrimaryConfigPath: string;
    FLazarusURL: string;
    FMakeDirectory: string;
    FOnlyModules: string;
    FShortCutNameLazarus: string;
    FShortCutNameFpcup: string;
    FSkipModules: string;
    FUninstall:boolean;
    FVerbose: boolean;
    Sequencer: TSequencer;
    function GetLazarusPrimaryConfigPath: string;
    function GetLogFileName: string;
    procedure SetBootstrapCompilerDirectory(AValue: string);
    procedure SetFPCDirectory(AValue: string);
    procedure SetFPCURL(AValue: string);
    procedure SetLazarusDirectory(AValue: string);
    procedure SetLazarusURL(AValue: string);
    procedure SetLogFileName(AValue: string);
    procedure SetMakeDirectory(AValue: string);
  protected
    FLog:TLogger;
    FVerboseLog:TLogger;
    FModuleList:TStringList;
    FModuleEnabledList:TStringList;
    FModulePublishedList:TStringList;
    // Write msg to log with line ending. Can also write to console
    procedure WritelnLog(msg:string;ToConsole:boolean=true);
 public
    property ShortCutNameLazarus: string read FShortCutNameLazarus write FShortCutNameLazarus; //Name of the shortcut that points to the fpcup-installed Lazarus
    property ShortCutNameFpcup:string read FShortCutNameFpcup write FShortCutNameFpcup; //Name of the shortcut that points to fpcup
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
    property FPCURL: string read FFPCURL write SetFPCURL;
    property FPCOPT: string read FFPCOPT write FFPCOPT;
    property FPCDesiredRevision:string read FFPCDesiredRevision write FFPCDesiredRevision;
    property KeepLocalChanges: boolean read FKeepLocalDiffs write FKeepLocalDiffs;
    property LazarusDirectory: string read FLazarusDirectory write SetLazarusDirectory;
    property LazarusPrimaryConfigPath: string read GetLazarusPrimaryConfigPath write FLazarusPrimaryConfigPath ;
    property LazarusURL: string read FLazarusURL write SetLazarusURL;
    property LazarusOPT:string read FLazarusOPT write FLazarusOPT;
    property LazarusDesiredRevision:string read FLazarusDesiredRevision write FLazarusDesiredRevision;
    // Location where fpcup log will be written to.
    property LogFileName: string read GetLogFileName write SetLogFileName;
    property MakeDirectory: string read FMakeDirectory write SetMakeDirectory;
    //List of all default enabled sequences available
    property ModuleEnabledList: TStringList read FModuleEnabledList;
    //List of all publicly visible sequences
    property ModulePublishedList: TStringList read FModulePublishedList;
    property SkipModules:string read FSkipModules write FSkipModules;
    property OnlyModules:string read FOnlyModules write FOnlyModules;
    property Uninstall: boolean read FUninstall write FUninstall;
    property Verbose:boolean read FVerbose write FVerbose;
    // Fill in ModulePublishedList and ModuleEnabledList and load other config elements
    function LoadFPCUPConfig:boolean;
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
    FLazarusPrimaryConfigPath:=ExcludeTrailingPathDelimiter(GetLocalAppDataPath())+DefaultPCPSubdir;
    {$ELSE}
    // Note: normal GetAppConfigDir gets ~/.config/fpcup/.lazarusdev or something
    // XdgConfigHome normally resolves to something like ~/.config
    // which is a reasonable default if we have no Lazarus primary config path set
    FLazarusPrimaryConfigPath:=ExcludeTrailingPathDelimiter(XdgConfigHome)+DefaultPCPSubdir;
    {$ENDIF MSWINDOWS}
  end;
  result:=FLazarusPrimaryConfigPath;
end;

function TFPCupManager.GetLogFileName: string;
begin
  result:=FLog.LogFile;
end;

procedure TFPCupManager.SetBootstrapCompilerDirectory(AValue: string);
begin
FBootstrapCompilerDirectory:=ExcludeTrailingPathDelimiter(ExpandFileName(AValue));
end;

procedure TFPCupManager.SetFPCDirectory(AValue: string);
begin
  FFPCDirectory:=ExpandFileName(AValue);
end;

procedure TFPCupManager.SetFPCURL(AValue: string);
begin
  if FFPCURL=AValue then Exit;
  if pos('//',AValue)>0 then
    FFPCURL:=AValue
  else
    FFPCURL:=installerUniversal.GetAlias(FConfigFile,'fpcURL',AValue);
end;

procedure TFPCupManager.SetLazarusDirectory(AValue: string);
begin
  FLazarusDirectory:=ExpandFileName(AValue);
end;

procedure TFPCupManager.SetLazarusURL(AValue: string);
begin
  if FLazarusURL=AValue then Exit;
  if pos('//',AValue)>0 then
    FLazarusURL:=AValue
  else
    FLazarusURL:=installerUniversal.GetAlias(FConfigFile,'lazURL',AValue);
end;

procedure TFPCupManager.SetLogFileName(AValue: string);
begin
  // Don't change existing log file
  if (AValue<>'') and (FLog.LogFile=AValue) then Exit;
  // Defaults if empty value specified
  if AValue='' then
    begin
    {$IFDEF MSWINDOWS}
    FLog.LogFile:='fpcup.log'; //current directory
    {$ELSE}
    FLog.LogFile:=ExpandFileNameUTF8('~')+DirectorySeparator+'fpcup.log'; //In home directory
    {$ENDIF MSWINDOWS}
    end
  else
    begin
    FLog.LogFile:=AValue;
    end;
end;

procedure TFPCupManager.SetMakeDirectory(AValue: string);
begin
  // Make directory can be empty (e.g. in Linux). In this case
  // expanding '' gives the user's home directory, which is not what we want.
  if AValue='' then
    FMakeDirectory:=''
  else
    FMakeDirectory:=ExpandFileName(AValue);
end;

procedure TFPCupManager.WritelnLog(msg: string; ToConsole: boolean);
begin
  // Set up log if it doesn't exist yet
  FLog.WriteLog(msg,ToConsole);
end;

function TFPCupManager.LoadFPCUPConfig: boolean;
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

  try
    WritelnLog(DateTimeToStr(now)+': fpcup '+RevisionStr+' ('+VersionDate+') started.',true);
  except
    // Writing to log failed, probably duplicate run. Inform user and get out.
    writeln('***ERROR***');
    writeln('Could not open log file '+FLog.LogFile+' for writing.');
    writeln('Perhaps another fpcup is running?');
    writeln('Aborting.');
    halt(2);
  end;

  if SkipModules<>'' then
    begin
    Sequencer.SkipList:=TStringList.Create;
    Sequencer.SkipList.Delimiter:=',';
    Sequencer.SkipList.DelimitedText:=SkipModules;
    end;
  if FOnlyModules<>'' then
    begin
    Sequencer.CreateOnly(FOnlyModules);
    result:=Sequencer.Run('Only');
    Sequencer.DeleteOnly;
    end
  else
    {$if defined(win32)}
    // Run Windows specific cross compiler or regular version
    if pos('CROSSWIN32-64',UpperCase(SkipModules))>0 then
      result:=Sequencer.Run('Default')
    else
      result:=Sequencer.Run('DefaultWin32');
    {
    // We would like to have a win64=>win32 crosscompiler, but at least with current
    // FPC trunk that won't work due to errors like
    // fpcdefs.inc(216,2) Error: User defined: Cross-compiling from systems
    // without support for an 80 bit extended floating point type to i386 is
    // not yet supported at this time
    {$elseif defined(win64)}
    if pos('CROSSWIN64-32',UpperCase(SkipModules))>0 then
      result:=Sequencer.Run('Default')
    else
      result:=Sequencer.Run('DefaultWin64');
    }
    {$else}
    // Linux, OSX
    result:=Sequencer.Run('Default');
    {$endif}
  if assigned(Sequencer.SkipList) then
    Sequencer.SkipList.Free;
end;

constructor TFPCupManager.Create;

begin
FModuleList:=TStringList.Create;
FModuleEnabledList:=TStringList.Create;
FModulePublishedList:=TStringList.Create;
Sequencer:=TSequencer.create;
Sequencer.Parent:=Self;
FLog:=TLogger.Create;
// Log filename will be set on first log write
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
  try
    WritelnLog(DateTimeToStr(now)+': fpcup finished.',true);
    WritelnLog('------------------------------------------------',false);
  finally
    //ignore logging errors
  end;
  FLog.Free;
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
  if FParent.ShortCutNameLazarus<>EmptyStr then
  begin
    infoln('Lazarus: creating desktop shortcut:',etinfo);
    try
      //Create shortcut; we don't care very much if it fails=>don't mess with OperationSucceeded
      InstalledLazarus:=IncludeTrailingPathDelimiter(FParent.LazarusDirectory)+'lazarus'+GetExeExt;
      {$IFDEF MSWINDOWS}
      CreateDesktopShortCut(InstalledLazarus,'--pcp="'+FParent.LazarusPrimaryConfigPath+'"',FParent.ShortCutNameLazarus);
      {$ENDIF MSWINDOWS}
      {$IFDEF UNIX}
      {$IFDEF DARWIN}
      CreateHomeStartLink(InstalledLazarus+'.app/Contents/MacOS/lazarus','--pcp="'+FParent.LazarusPrimaryConfigPath+'"',FParent.ShortcutNameLazarus);
      {$ELSE}
      CreateHomeStartLink(InstalledLazarus,'--pcp="'+FParent.LazarusPrimaryConfigPath+'"',FParent.ShortcutNameLazarus);
      {$ENDIF DARWIN}
      {$IFDEF LINUX}
      // Desktop shortcut creation will not always work. As a fallback, create the link in the home directory:
      CreateDesktopShortCut(InstalledLazarus,'--pcp="'+FParent.LazarusPrimaryConfigPath+'"',FParent.ShortCutNameLazarus);
      CreateHomeStartLink(InstalledLazarus,'--pcp="'+FParent.LazarusPrimaryConfigPath+'"',FParent.ShortcutNameLazarus);
      {$ENDIF LINUX}
      {$ENDIF UNIX}
    finally
      //Ignore problems creating shortcut
    end;
  end;
  end;

  function DeleteLazarusScript:boolean;
  begin
  result:=true;
  if FParent.ShortCutNameLazarus<>EmptyStr then
  begin
    infoln('Lazarus: deleting desktop shortcut:',etinfo);
    try
      //Delete shortcut; we don't care very much if it fails=>don't mess with OperationSucceeded
      {$IFDEF MSWINDOWS}
      DeleteDesktopShortCut(FParent.ShortCutNameLazarus);
      {$ENDIF MSWINDOWS}
      {$IFDEF UNIX}
      DeleteFileUTF8(FParent.ShortcutNameLazarus);
      {$ENDIF UNIX}
    finally
      //Ignore problems creating shortcut
    end;
  end;
  end;


  {$ifdef linux}
  function CheckDevLibs(LCLPlatform: string): boolean;
  const
    LIBSCNT=4;
  type
    TLibList=array[1..LIBSCNT] of string;
  const
    LCLLIBS:TLibList = ('libX11.so','libgdk_pixbuf-2.0.so','libpango-1.0.so','libgdk-x11-2.0.so');
    QTLIBS:TLibList = ('libQt4Pas.so','','','');
  var
    i:integer;
    pll:^TLibList;

    function TestLib(LibName:string):boolean;
    var
      Lib : TLibHandle;
    begin
      result:=true;
      if LibName<>'' then
        begin
        Lib:=LoadLibrary(LibName);
        result:=Lib<>0;
        if result then
          UnloadLibrary(Lib);
        end;
    end;

  begin
    result:=true;
    if (LCLPlatform='') or (Uppercase(LCLPlatform)='GTK2') then
      pll:=@LCLLIBS
    else if Uppercase(LCLPlatform)='QT' then
      pll:=@QTLIBS;
    for i:=1 to LIBSCNT do
      begin
      if not TestLib(pll^[i]) then
        begin
        FParent.WritelnLog('Required -dev packages are not installed for Lazarus: '+pll^[i], true);
        result:=false;
        end;
      end;
  end;
  {$endif linux}

begin
  if UpperCase(FunctionName)='CREATEFPCUPSCRIPT' then
    result:=CreateFpcupScript
  else if UpperCase(FunctionName)='CREATELAZARUSSCRIPT' then
    result:=CreateLazarusScript
  else if UpperCase(FunctionName)='DELETELAZARUSSCRIPT' then
    result:=DeleteLazarusScript
  {$ifdef linux}
  else if UpperCase(FunctionName)='CHECKDEVLIBS' then
    result:=CheckDevLibs(FParent.CrossLCL_Platform)
  {$endif linux}
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
  result:=true;
  CrossCompiling:= (FParent.CrossCPU_Target<>'') or (FParent.CrossOS_Target<>'');
  //check if this is a known module

  // FPC:
  if uppercase(ModuleName)='FPC' then
    begin
    if assigned(Installer) then
      begin
      // Check for existing normal compiler, or exact same cross compiler
      if (not crosscompiling and (Installer is TFPCNativeInstaller)) or
        ( crosscompiling and
        (Installer is TFPCCrossInstaller) and
        (Installer.CrossOS_Target=FParent.CrossOS_Target) and
        (Installer.CrossCPU_Target=FParent.CrossCPU_Target)
        ) then
        begin
        exit; //all fine, continue with current installer
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
    Installer.URL:=FParent.FPCURL;
    end

  // Lazarus:
  else if (uppercase(ModuleName)='LAZARUS') or (uppercase(ModuleName)='LAZBUILD') or (uppercase(ModuleName)='LCL') or
    (uppercase(ModuleName)='BIGIDE') or (uppercase(ModuleName)='USERIDE') then
    begin
    if assigned(Installer) then
      begin
      if (not crosscompiling and (Installer is TLazarusNativeInstaller)) or
        ( crosscompiling and (Installer is TLazarusCrossInstaller)) then
        begin
        exit; //all fine, continue with current installer
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
    (Installer As TLazarusInstaller).PrimaryConfigPath:=FParent.LazarusPrimaryConfigPath;
    Installer.URL:=FParent.LazarusURL;
    end

  //Convention: help modules start with HelpFPC
  //or HelpLazarus
  else if uppercase(ModuleName)='HELPFPC' then
      begin
      if assigned(Installer) then
        begin
        if (Installer is THelpFPCInstaller) then
          begin
          exit; //all fine, continue with current installer
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
      end

  else if uppercase(ModuleName)='HELPLAZARUS' then
      begin
      if assigned(Installer) then
        begin
       if (Installer is THelpLazarusInstaller) then
          begin
          exit; //all fine, continue with current installer
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
      (Installer as THelpLazarusInstaller).LazarusPrimaryConfigPath:=FParent.LazarusPrimaryConfigPath;
      end

  else       // this is a universal module
    begin
      if assigned(Installer) then
        begin
        if (Installer is TUniversalInstaller) and
          (CurrentModule= ModuleName) then
          begin
          exit; //all fine, continue with current installer
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
      if FParent.CompilerName='' then
        Installer.Compiler:=Installer.GetCompilerInDir(FParent.FPCDirectory)
      else
        Installer.Compiler:=FParent.CompilerName;
    end;

  Installer.KeepLocalChanges:=FParent.KeepLocalChanges;
  Installer.Verbose:=FParent.Verbose;
  Installer.Log:=FParent.FLog;
  {$IFDEF MSWINDOWS}
  Installer.MakeDirectory:=FParent.MakeDirectory;
  {$ENDIF}
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

