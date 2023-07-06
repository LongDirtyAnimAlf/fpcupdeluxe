unit processutils;

{$mode objfpc}{$H+}

{$i fpcupdefines.inc}

interface

uses
  Classes, SysUtils,
  Process;

const
  Seriousness: array [TEventType] of string = ('custom:', 'info:', 'WARNING:', 'ERROR:', 'debug:');
  {$ifdef LCL}
  BeginSnippet='fpcupdeluxe:'; //helps identify messages as coming from fpcupdeluxe instead of make etc
  {$else}
  {$ifndef FPCONLY}
  BeginSnippet='fpclazup:'; //helps identify messages as coming from fpclazup instead of make etc
  {$else}
  BeginSnippet='fpcup:'; //helps identify messages as coming from fpcup instead of make etc
  {$endif}
  {$endif}

  {$IFDEF MSWINDOWS}
  PATHVARNAME = 'Path'; //Name for path environment variable
  {$ELSE}
  //Unix/Linux
  PATHVARNAME = 'PATH';
  {$ENDIF MSWINDOWS}

resourcestring
  lisExitCode = 'Exit code %s';
  lisToolHasNoExecutable = 'tool "%s" has no executable';
  lisCanNotFindExecutable = 'cannot find executable "%s"';
  lisMissingExecutable = 'missing executable "%s"';
  lisExecutableIsADirectory = 'executable "%s" is a directory';
  lisExecutableLacksThePermissionToRun = 'executable "%s" lacks the permission to run';
  lisSuccess = 'Success';
  lisAborted = 'Aborted';
  lisCanNotExecute = 'cannot execute "%s"';
  lisMissingDirectory = 'missing directory "%s"';
  lisUnableToExecute = 'unable to execute: %s';
  lisUnableToReadProcessExitStatus = 'unable to read process ExitStatus';
  lisFreeingBufferLines = 'freeing buffer lines: %s';

const
  AbortedExitCode = 12321;

type
  { TProcessEnvironment }

  TProcessEnvironment = class(TObject)
  private
    FEnvironmentList:TStringList;
    FCaseSensitive:boolean;
    function GetVarIndex(VarName:string):integer;
  public
    // Get environment variable
    function GetVar(VarName:string):string;
    // Set environment variable
    procedure SetVar(VarName,VarValue:string);
    // List of all environment variables (name and value)
    property EnvironmentList:TStringList read FEnvironmentList;
    constructor Create;
    destructor Destroy; override;
  end;

  TExternalToolStage = (
    etsInit,            // just created, set your parameters, then call Execute
    etsInitializing,    // set in Execute, during resolving macros
    etsWaitingForStart, // waiting for a process slot
    etsStarting,        // creating the thread and process
    etsRunning,         // process started
    etsWaitingForStop,  // waiting for process to stop
    etsStopped,         // process has stopped
    etsDestroying       // during destructor
    );
  TExternalToolStages = set of TExternalToolStage;

  TExternalToolNewOutputEvent = procedure(Sender: TObject;
                                          FirstNewMsgLine: integer) of object;

  TExternalToolHandler = (
    ethNewOutput,
    ethStopped
    );

  TOnUpdateEvent = procedure(Sender: TObject;Status:TExternalToolStage) of object;

  TAbstractExternalTool = class(TComponent)
  private
    FCritSec: TRTLCriticalSection;
    FData: TObject;
    FExitCode: integer;
    FExitStatus: integer;
    FFreeData: boolean;
    FReadStdOutBeforeErr: boolean;
    FTitle: string;
    FProcessEnvironment:TProcessEnvironment;
    FCmdLineExe: string;
    FOnUpdateEvent: TOnUpdateEvent;
    function GetCmdLineParams: string;
    procedure SetCmdLineParams(aParams: string);
    procedure SetCmdLineExe(aExe: string);
    procedure SetTitle(const AValue: string);
    procedure UpdateEvent(Sender : TObject;Status:TExternalToolStage);
  protected
    FErrorMessage: string;
    FTerminated: boolean;
    FStage: TExternalToolStage;
    FWorkerOutput: TStringList;
    FProcess: TProcess;
    function GetProcessEnvironment: TProcessEnvironment;
    procedure DoExecute; virtual; abstract;
    function CanFree: boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EnterCriticalSection;
    procedure LeaveCriticalSection;
    procedure AutoFree;

    property Title: string read FTitle write SetTitle;
    property Data: TObject read FData write FData;
    property FreeData: boolean read FFreeData write FFreeData default false;

    // process
    property Process: TProcess read FProcess;
    property Executable: string read FCmdLineExe write SetCmdLineExe;
    property CmdLineParams: string read GetCmdLineParams write SetCmdLineParams;
    property Stage: TExternalToolStage read FStage;
    procedure Execute; virtual; abstract;
    procedure Terminate; virtual; abstract;
    procedure WaitForExit; virtual; abstract;
    property Terminated: boolean read FTerminated;
    property ExitCode: integer read FExitCode write FExitCode;
    property ExitStatus: integer read FExitStatus write FExitStatus;
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
    property ReadStdOutBeforeErr: boolean read FReadStdOutBeforeErr write FReadStdOutBeforeErr;
    property Environment:TProcessEnvironment read GetProcessEnvironment;
    Property OnUpdateEvent : TOnUpdateEvent Read FOnUpdateEvent Write FOnUpdateEvent;

    procedure SetParamMakefilePathData(const aName,aValue:string);
    procedure SetParamData(const aValue:string);


    // output
    property WorkerOutput: TStringList read FWorkerOutput; // the raw output
  end;

  TExternalTool = class;

  { TExternalToolThread }

  {$ifdef THREADEDEXECUTE}
  TExternalToolThread = class(TThread)
  {$else}
  TExternalToolThread = class(TObject)
  {$endif}
  private
    fLines: TStringList;
    FTool: TExternalTool;
    procedure SetTool(AValue: TExternalTool);
    function GetFilter(line: string; aVerbosity:boolean):boolean;
  public
    property Tool: TExternalTool read FTool write SetTool;
    {$ifdef THREADEDEXECUTE}
    procedure Execute; override;
    {$else}
    procedure Execute;
    {$endif}
    destructor Destroy; override;
  end;

  { TExternalTool }

  TExternalTool = class(TAbstractExternalTool)
  private
    FThread: TExternalToolThread;
    FVerbose:boolean;
    procedure ProcessRunning;
    procedure ProcessStopped;
    procedure AddOutputLines(Lines: TStringList);
    procedure SetThread(AValue: TExternalToolThread);
    procedure DoTerminate;
    procedure SyncAutoFree({%H-}aData: PtrInt=0);
  protected
    FFPCMagic:boolean; // tricky filtering
    procedure DoExecute; override;
    procedure DoStart;
    function CanFree: boolean; override;
    procedure QueueAsyncAutoFree;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property Thread: TExternalToolThread read FThread write SetThread;
    property Verbose: boolean read FVerbose write FVerbose;
    procedure Execute; override;
    procedure Terminate; override;
    procedure WaitForExit; override;
    function GetExeInfo:string;
    function CanStart: boolean;
    function ExecuteAndWait:integer;
  end;

  procedure ThreadLog(const aMsg: string;{%H-}const aEvent:TEventType=etInfo);

implementation

uses
  {$ifdef LCL}
  Forms,
  Controls, // for crHourGlass
  LCLIntf,
  LCLType,
  LMessages,
  {$endif}
  StrUtils,
  Pipes,
  Math,
  FileUtil,
  LazFileUtils;


{ TProcessEnvironment }

function TProcessEnvironment.GetVarIndex(VarName: string): integer;
var
  idx:integer;

  function ExtractVar(VarVal:string):string;
  begin
    result:='';
    if length(Varval)>0 then
      begin
      if VarVal[1] = '=' then //windows
        delete(VarVal,1,1);
      result:=trim(copy(VarVal,1,pos('=',VarVal)-1));
      if not FCaseSensitive then
        result:=UpperCase(result);
      end
  end;

begin
  if (Length(VarName)=0) then
  begin
    result:=-1;
  end
  else
  begin
    if not FCaseSensitive then
      VarName:=UpperCase(VarName);
    idx:=0;
    while idx<FEnvironmentList.Count  do
    begin
      if VarName = ExtractVar(FEnvironmentList[idx]) then
        break;
      idx:=idx+1;
    end;
    if idx<FEnvironmentList.Count then
      result:=idx
    else
      result:=-1;
  end;
end;

function TProcessEnvironment.GetVar(VarName: string): string;
var
  idx:integer;

  function ExtractVal(VarVal:string):string;
  begin
    result:='';
    if length(Varval)>0 then
      begin
      if VarVal[1] = '=' then //windows
        delete(VarVal,1,1);
      result:=trim(copy(VarVal,pos('=',VarVal)+1,length(VarVal)));
      end
  end;

begin
  idx:=GetVarIndex(VarName);
  if idx>=0 then
    result:=ExtractVal(FEnvironmentList[idx])
  else
    result:='';
end;

procedure TProcessEnvironment.SetVar(VarName, VarValue: string);
var
  idx:integer;
  s:string;
begin
  if (Length(VarName)=0) then exit;
  idx:=GetVarIndex(VarName);
  if (idx>=0) AND (Length(VarValue)=0) then
  begin
    FEnvironmentList.Delete(idx);
  end
  else
  if (Length(VarValue)>0) then
  begin
    s:=trim(Varname)+'='+trim(VarValue);
    if idx>=0 then
      FEnvironmentList[idx]:=s
    else
      FEnvironmentList.Add(s);
  end;
end;

constructor TProcessEnvironment.Create;
var
  i: integer;
begin
  FEnvironmentList:=TStringList.Create;
  {$ifdef WINDOWS}
  FCaseSensitive:=false;
  {$else}
  FCaseSensitive:=true;
  {$endif WINDOWS}
  // GetEnvironmentVariableCount is 1 based
  for i:=1 to GetEnvironmentVariableCount do
    EnvironmentList.Add(TrimLeft(GetEnvironmentString(i)));
end;

destructor TProcessEnvironment.Destroy;
begin
  FEnvironmentList.Free;
  inherited Destroy;
end;

{ TAbstractExternalTool }

function TAbstractExternalTool.GetCmdLineParams: string;
var
  i: Integer;
begin
  Result:='';
  if Process.Parameters=nil then exit;
  for i:=0 to Pred(Process.Parameters.Count) do
  begin
    if i>0 then Result+=' ';
    Result:=Result+Process.Parameters[i];
  end;
end;

procedure TAbstractExternalTool.SetCmdLineParams(aParams: string);
var
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    SplitCmdLineParams(aParams,sl);
    Process.Parameters:=sl;
  finally
    sl.Free;
  end;
end;

procedure TAbstractExternalTool.SetCmdLineExe(aExe: string);
begin
  FCmdLineExe:=aExe;
  Process.Executable:=FCmdLineExe;
end;

procedure TAbstractExternalTool.SetTitle(const AValue: string);
begin
  if FTitle=AValue then exit;
  FTitle:=AValue;
end;

procedure TAbstractExternalTool.UpdateEvent(Sender: TObject;Status:TExternalToolStage);
begin
  if MainThreadID=ThreadID then
  begin
    //if IsMultiThread then
    {$ifdef LCL}
    Application.ProcessMessages;
    {$else}
    CheckSynchronize(0);
    {$endif}
  end;
  //if status=etsRunning then
  //  //sleep(Process.RunCommandSleepTime);
  //  sleep(10);
end;


function TAbstractExternalTool.CanFree: boolean;
begin
  Result:=false;
  if csDestroying in ComponentState then exit;
  if (Process<>nil) and (Process.Running) then
    exit;
  Result:=true;
end;

constructor TAbstractExternalTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStage:=etsInit;
  InitCriticalSection(FCritSec);
end;

destructor TAbstractExternalTool.Destroy;
begin
  EnterCriticalSection;
  try
    if FreeData then FreeAndNil(FData);
    if assigned(FProcessEnvironment) then FProcessEnvironment.Free;
  finally
    LeaveCriticalsection;
  end;
  DoneCriticalSection(FCritSec);
  inherited Destroy;
end;

procedure TAbstractExternalTool.EnterCriticalSection;
begin
  System.EnterCriticalsection(FCritSec);
end;

procedure TAbstractExternalTool.LeaveCriticalSection;
begin
  System.LeaveCriticalsection(FCritSec);
end;

procedure TAbstractExternalTool.AutoFree;
begin
  if MainThreadID<>GetCurrentThreadId then
    raise Exception.Create('AutoFree only via main thread');
  if CanFree then
    Free;
end;

function TAbstractExternalTool.GetProcessEnvironment: TProcessEnvironment;
begin
  If not assigned(FProcessEnvironment) then
    FProcessEnvironment:=TProcessEnvironment.Create;
  result:=FProcessEnvironment;
end;

{ TExternalTool }

procedure TAbstractExternalTool.SetParamMakefilePathData(const aName,aValue:string);
var
  aCorrectValue:string;
  i:integer;
begin
  if Assigned(Process) then
  begin
    aCorrectValue:=aValue;
    {$ifdef Windows}
    if (Length(aCorrectValue)>0) then
    begin
      if (Pos(' ',aCorrectValue)>0) then aCorrectValue:=ExtractShortPathName(aCorrectValue);
      for i:=1 to Length(aCorrectValue) do
        if (aCorrectValue[i]=DirectorySeparator) then
          aCorrectValue[i]:='/';
    end;
    {$endif}
    if (Length(aCorrectValue)=0) then
    begin
      i:=Process.Parameters.IndexOf(aName);
      if (i<>-1) then TProcessStringList(Process.Parameters).Delete(i);
    end
    else
      Process.Parameters.Values[aName]:=aCorrectValue;
  end;
end;

procedure TAbstractExternalTool.SetParamData(const aValue:string);
begin
  if Assigned(Process) then
  begin
    if (Length(aValue)>0) then
    begin
      Process.Parameters.Append(aValue);
    end;
  end;
end;

procedure TExternalTool.ProcessRunning;
begin
  EnterCriticalSection;
  try
    if FStage<>etsStarting then exit;
    FStage:=etsRunning;
  finally
    LeaveCriticalSection;
  end;
end;

procedure TExternalTool.ProcessStopped;
begin
  EnterCriticalSection;
  try
    if (not Terminated) and (ErrorMessage='') then
    begin
      if ExitCode<>0 then
        ErrorMessage:=Format(lisExitCode, [IntToStr(ExitCode)])
      else if ExitStatus<>0 then
        ErrorMessage:='ExitStatus '+IntToStr(ExitStatus);
    end;
    if FStage>=etsStopped then exit;
    if Assigned(FProcessEnvironment) then FProcessEnvironment.Destroy;
    FProcessEnvironment:=nil;
    FStage:=etsStopped;
  finally
    LeaveCriticalSection;
  end;
  {$ifndef THREADEDEXECUTE}
  Thread.Destroy;
  {$endif}
  fThread:=nil;
end;

procedure TExternalTool.AddOutputLines(Lines: TStringList);
var
  Line: LongInt;
  OldOutputCount: LongInt;
  LineStr: String;
begin
  if (Lines=nil) or (Lines.Count=0) then exit;
  EnterCriticalSection;
  try
    OldOutputCount:=WorkerOutput.Count;
    WorkerOutput.AddStrings(Lines);
    for Line:=OldOutputCount to WorkerOutput.Count-1 do
    begin
      LineStr:=WorkerOutput[Line];
      if IsMultiThread then
      begin
      end;
      if Verbose OR FFPCMagic
      //OR (NOT IsMultiThread)
      //{$ifndef LCL} OR True{$endif}
      {$ifdef DEBUG} OR True{$endif}
      then
      begin
        ThreadLog(LineStr);
      end;
    end;
  finally
    LeaveCriticalSection;
  end;
end;

procedure TExternalTool.SetThread(AValue: TExternalToolThread);
var
  CallAutoFree: Boolean;
begin
  // Note: in lazbuild ProcessStopped sets FThread:=nil, so SetThread is not called.
  EnterCriticalSection;
  try
    if FThread=AValue then Exit;
    FThread:=AValue;
    CallAutoFree:=CanFree;
  finally
    LeaveCriticalSection;
  end;
  if CallAutoFree then
  begin
    if MainThreadID=GetCurrentThreadId then
      AutoFree
    else
      QueueAsyncAutoFree;
  end;
end;

constructor TExternalTool.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FWorkerOutput:=TStringList.Create;
  FProcess:=TProcess.Create(nil);
  //FProcess:=DefaultTProcess.Create(nil);
  //Process.Options:= [poUsePipes{$IFDEF Windows},poStderrToOutPut{$ENDIF}];
  //Process.Options := FProcess.Options +[poUsePipes, poStderrToOutPut];
  Process.Options:= [{poWaitOnExit,}poUsePipes{$ifdef Windows},poStderrToOutPut{$endif}];
  //Process.Options := FProcess.Options +[poUsePipes, poStderrToOutPut]-[poRunSuspended,poWaitOnExit];
  {$ifdef LCL}
  FProcess.ShowWindow := swoHide;
  {$endif}
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)}
  Process.RunCommandSleepTime:=10; // rest the default sleep time to 0 (context switch only)
  {$ENDIF}
  Self.OnUpdateEvent:=@UpdateEvent;
  FVerbose:=true;
end;

destructor TExternalTool.Destroy;
begin
  EnterCriticalSection;
  try
    FStage:=etsDestroying;
    if Thread is TExternalToolThread then
      TExternalToolThread(Thread).Tool:=nil;
    FreeAndNil(FProcess);
    FreeAndNil(FWorkerOutput);
  finally
    LeaveCriticalSection;
  end;
  inherited Destroy;
end;

procedure TExternalTool.DoExecute;
// in main thread

  function CheckError: boolean;
  begin
    if (FStage>=etsStopped) then exit(true);
    if (ErrorMessage='') then exit(false);
    EnterCriticalSection;
    try
      if FStage>=etsStopped then exit(true);
      FStage:=etsStopped;
    finally
      LeaveCriticalSection;
    end;
    Result:=true;
  end;

var
  ExeFile: String;
begin
  if Terminated then exit;

  EnterCriticalSection;
  try
    if Stage<>etsInit then
      raise Exception.Create('TExternalTool.Execute: already initialized');
    FStage:=etsInitializing;
    WorkerOutput.Clear;
  finally
    LeaveCriticalSection;
  end;


  // init CurrentDirectory
  Process.CurrentDirectory:=TrimFilename(Process.CurrentDirectory);
  if not FilenameIsAbsolute(Process.CurrentDirectory) then
    Process.CurrentDirectory:=AppendPathDelim(GetCurrentDir)+Process.CurrentDirectory;

  // init Executable
  Process.Executable:=TrimFilename(Process.Executable);
  if not FilenameIsAbsolute(Process.Executable) then
  begin
    if ExtractFilePath(Process.Executable)<>'' then
      Process.Executable:=AppendPathDelim(GetCurrentDir)+Process.Executable
    else if Process.Executable='' then
    begin
      ErrorMessage:=Format(lisToolHasNoExecutable, [Title]);
      CheckError;
      exit;
    end else begin
      ExeFile:=FindDefaultExecutablePath(Process.Executable,GetCurrentDir);
      if ExeFile='' then
      begin
        ErrorMessage:=Format(lisCanNotFindExecutable, [Process.Executable]);
        CheckError;
        exit;
      end;
      Process.Executable:=ExeFile;
    end;
  end;
  ExeFile:=Process.Executable;
  if not FileExists(ExeFile) then
  begin
    ErrorMessage:=Format(lisMissingExecutable, [ExeFile]);
    CheckError;
    exit;
  end;
  if DirectoryExists(ExeFile) then
  begin
    ErrorMessage:=Format(lisExecutableIsADirectory, [ExeFile]);
    CheckError;
    exit;
  end;
  if not FileIsExecutable(ExeFile) then
  begin
    ErrorMessage:=Format(lisExecutableLacksThePermissionToRun, [ExeFile]);
    CheckError;
    exit;
  end;

  //Do we have something FPC or Lazarus or fpcupdeluxe like. If so, apply some filtering when not Verbose
  //Filtering is dome here to limit the amount of thread message traffic
  //Bit tricky ... ;-)
  FFPCMagic:=False;
  if (NOT Verbose) then
  begin
    ExeFile:=LowerCase(ExtractFileName(Process.Executable));

    // Show exe info to the user.
    ThreadLog(GetExeInfo,etCustom);

    if
      (
      (Pos('fpc',ExeFile)=1)
      OR (Pos('ppc',ExeFile)=1)
      OR (Pos('lazbuild',ExeFile)=1)
      OR (Pos('make',ExeFile)=1)
      OR (Pos('gmake',ExeFile)=1)
      )
    then
    begin
      FFPCMagic:=True;
    end;
  end;

  // init misc
  if Assigned(FProcessEnvironment) then
      Process.Environment:=FProcessEnvironment.EnvironmentList;

  EnterCriticalSection;
  try
    if Stage<>etsInitializing then
      raise Exception.Create('TExternalTool.Execute: bug in initialization');
    FStage:=etsWaitingForStart;
  finally
    LeaveCriticalSection;
  end;
end;

procedure TExternalTool.DoStart;
begin
  EnterCriticalSection;
  try
    if Stage<>etsWaitingForStart then
      raise Exception.Create('TExternalTool.Execute: already started');
    FStage:=etsStarting;
  finally
    LeaveCriticalSection;
  end;

  {$ifdef THREADEDEXECUTE}
  if Thread=nil then
  begin
    FThread:=TExternalToolThread.Create(true);
    Thread.Tool:=Self;
    Thread.FreeOnTerminate:=true;
  end;
  Thread.Start;
  {$else}
  if Thread=nil then
  begin
    FThread:=TExternalToolThread.Create;
    Thread.Tool:=Self;
  end;
  Thread.Execute;
  {$endif}
end;

procedure TExternalTool.DoTerminate;
var
  NeedProcTerminate: Boolean;
begin
  NeedProcTerminate:=false;
  EnterCriticalSection;
  try
    if Terminated then exit;
    if Stage=etsStopped then exit;

    if ErrorMessage='' then
      ErrorMessage:=lisAborted;
    fTerminated:=true;
    if Stage=etsRunning then
      NeedProcTerminate:=true;
    if Stage<etsStarting then
      FStage:=etsStopped
    else if Stage<=etsRunning then
      FStage:=etsWaitingForStop;
  finally
    LeaveCriticalSection;
  end;
  if NeedProcTerminate and (Process<>nil) then
  begin
    Process.Terminate(AbortedExitCode);
    {$IF FPC_FULLVERSION < 30300}
    Process.WaitOnExit;
    {$ELSE}
    Process.WaitOnExit(5000);
    {$ENDIF}
    //To check !!
    //fTerminated:=false;
  end;
end;

function TExternalTool.CanFree: boolean;
begin
  Result:=(FThread=nil) and inherited CanFree;
end;

procedure TExternalTool.SyncAutoFree(aData: PtrInt);
begin
  AutoFree;
end;

procedure TExternalTool.QueueAsyncAutoFree;
begin
  {$ifdef LCL}
  Application.QueueAsyncCall(@SyncAutoFree,0);
  {$endif}
end;

function TExternalTool.CanStart: boolean;
begin
  Result:=false;
  if Stage<>etsWaitingForStart then exit;
  if Terminated then exit;
  Result:=true;
end;

procedure TExternalTool.Execute;
begin
  if Stage<>etsInit then
  begin
    if Stage=etsStopped then
    begin
      EnterCriticalSection;
      try
        FStage:=etsInit;
      finally
        LeaveCriticalSection;
      end;
    end else raise Exception.Create('TExternalTool.Execute "'+Title+'" already started');
  end;
  DoExecute;
  if Stage<>etsWaitingForStart then
    exit
  else
    DoStart;
end;

procedure TExternalTool.Terminate;
begin
  DoTerminate;
end;

procedure TExternalTool.WaitForExit;
begin
  repeat
    try
      EnterCriticalSection;
      try
        if Stage=etsDestroying then break;
        if Stage=etsStopped then break;
        // still running => wait a bit to prevent cpu cycle burning
      finally
        LeaveCriticalSection;
      end;
    finally
      //WakeMainThread;
      //ThreadSwitch;
      if MainThreadID=ThreadID then
      begin
        //if IsMultiThread then
        {$ifdef LCL}
        Application.ProcessMessages;
        {$else}
        CheckSynchronize(0); // if we use Thread.Synchronize
        {$endif}
        //TExternalToolsBase(Owner).HandleMesages;
      end;
    end;
    sleep(10)
  until false;
end;

function TExternalTool.GetExeInfo:string;
begin
  result:='Executing: '+Process.Executable+' '+CmdLineParams+' (working dir: '+ Process.CurrentDirectory +')';
end;

function TExternalTool.ExecuteAndWait:integer;
begin
  result:=-1;
  Execute;
  WaitForExit;
  //result:=ExitCode;
  result:=ExitStatus;
  //result:=(ErrorMessage='') and (not Terminated) and (ExitStatus=0);
end;


{ TExternalToolThread }

function TExternalToolThread.GetFilter(line: string; aVerbosity:boolean):boolean;
var
  s:string;
begin
  result:=false;

  // skip stray empty lines
  if (Length(line)=0) then exit;

  {$ifdef Darwin}
  // suppress all setfocus errors on Darwin, always
  if AnsiContainsText(line,'.setfocus') then exit;
  {$endif}

  {$ifdef Unix}
  // suppress all Kb Used messages, always
  if AnsiContainsText(line,'Kb Used') then exit;
  {$endif}

  // suppress all SynEdit PaintLock errors, always
  if AnsiContainsText(line,'PaintLock') then exit;

  // suppress some GIT errors, always
  if AnsiContainsText(line,'fatal: not a git repository') then exit;
  if AnsiStartsText('fatal: No names found',line) then exit;

  //Haiku errors we are not interested in
  {$ifdef Haiku}
  if AnsiStartsText('runtime_loader:',line) then exit;
  {$endif}

  // suppress some lazbuild errors, always
  if AnsiContainsText(line,'lazbuild') then
  begin
    if AnsiContainsText(line,'only for runtime') then exit;
    if AnsiContainsText(line,'lpk file expected') then exit;
  end;

  //Makefile error(s) we are not interested in
  if AnsiContainsText(line,'CreateProcess(') then exit;
  if AnsiContainsText(line,'make') then
  begin
    if AnsiContainsText(line,'error 87') then exit;
    if AnsiContainsText(line,'(e=87)') then exit;
    if AnsiContainsText(line,':294:') then exit;
  end;

  //Various harmless OpenBSD errors
  if AnsiContainsText(line,'misused, please use') then exit;
  if AnsiContainsText(line,'may return deterministic values') then exit;
  if AnsiContainsText(line,'dangerous; do not use it') then exit;
  if AnsiContainsText(line,'may conflict with') then exit;

  //Various FPC messags
  if AnsiStartsText('Compiling Release Version',line) then exit;
  if AnsiStartsText('Compiling Debug Version',line) then exit;

  //Skip debug message
  //if AnsiStartsText('TExternalToolsConsole.HandleMesages: Calling CheckSynchronize!',line) then exit;
  if AnsiStartsText('TExternalToolsConsole',line) then exit;

  // Harmless GIT warning
  if AnsiStartsText('warning: redirecting to https',line) then exit;

  result:=(NOT aVerbosity);

  if (NOT result) then
  begin
    //Harmless Jasmin error(s)
    if AnsiContainsText(line,'Badly formatted number') then exit;
    if AnsiContainsText(line,'system.j:') then exit;
    if AnsiStartsText('^',line) then exit;

    //Harmless linker warning
    if AnsiContainsText(line,'did you forget -T') then exit;

    //Harmless fpmkpkg warning
    if AnsiStartsText('Could not find libgcc ',line) then exit;
  end;

  if (NOT result) then
  begin

    {$ifdef Darwin}
    // Skip harmless error on Darwin
    //if AnsiStartsText('cannot execute a binary file',line) then exit;
    //if AnsiStartsText('could not find libgcc',line) then exit;
    if AnsiStartsText('svnversion: error:',line) then exit;
    {$endif Darwin}

    // GIT quirks.
    if AnsiStartsText('fatal: No annotated tags ',line) then exit;
    if AnsiStartsText('fatal: HEAD does not ',line) then exit;
    if AnsiStartsText('fatal: no tag exactly matches',line) then exit;
    if AnsiStartsText('fatal: unknown date format',line) then exit;

    if AnsiStartsText('However, ',line) then exit;

    // to be absolutely sure not to miss errors and fatals and fpcupdeluxe messages !!
    // will be a bit redundant , but just to be sure !
    if (AnsiContainsText(line,'error:'))
       OR (AnsiContainsText(line,'donalf:'))
       OR (AnsiContainsText(line,'fatal:'))
       OR (AnsiContainsText(line,'fpcupdeluxe:'))
       OR (AnsiContainsText(line,'execute:'))
       OR (AnsiContainsText(line,'executing:'))
       OR ((AnsiContainsText(line,'compiling ')) AND (NOT AnsiContainsText(line,'when compiling target')))
       OR ((AnsiContainsText(line,'/ppcross')) AND (AnsiContainsText(line,'/fpc/bin/')))
       OR (AnsiContainsText(line,'linking '))
    then result:=true;

    if (NOT result) then
    begin
      //Exttools debugging
      if AnsiStartsText('TExternalTool',line) then exit;

      // remove hints and other "trivial"* warnings from output
      // these line are not that interesting for the average user of fpcupdeluxe !
      if AnsiContainsText(line,'hint: ') then exit;
      if AnsiContainsText(line,'verbose: ') then exit;
      if AnsiContainsText(line,'note: ') then exit;

      // harmless make error
      if AnsiContainsText(line,'make') then
      begin
        if AnsiContainsText(line,'error 1') then exit;
        if AnsiContainsText(line,'(e=1)') then exit;
        if AnsiContainsText(line,'-iTP:') then exit;
        //if AnsiContainsText(line,'dependency dropped') then exit;
      end;

      if AnsiContainsText(line,'~~~~~~~~') then exit;
      if AnsiContainsText(line,', coalesced') then exit;

      if AnsiContainsText(line,'TODO: ') then exit;

      // When building a java cross-compiler
      if AnsiContainsText(line,'Generated: ') then exit;

      // filter warnings
      if AnsiContainsText(line,'warning:') then
      begin
        if AnsiContainsText(line,'is not portable') then exit;
        if AnsiContainsText(line,'is deprecated') then exit;
        if AnsiContainsText(line,'implicit string type conversion') then exit;
        if AnsiContainsText(line,'function result does not seem to be set') then exit;
        if AnsiContainsText(line,'comparison might be always') then exit;
        if AnsiContainsText(line,'converting pointers to signed integers') then exit;
        if AnsiContainsText(line,'does not seem to be initialized') then exit;
        if AnsiContainsText(line,'an inherited method is hidden') then exit;
        if AnsiContainsText(line,'with abstract method') then exit;
        if AnsiContainsText(line,'comment level 2 found') then exit;
        if AnsiContainsText(line,'is not recommended') then exit;
        if AnsiContainsText(line,'were not initialized') then exit;
        if AnsiContainsText(line,'which is not available for the') then exit;
        if AnsiContainsText(line,'argument unused during compilation') then exit;
        if AnsiContainsText(line,'invalid unitname') then exit;
        if AnsiContainsText(line,'procedure type "FAR" ignored') then exit;
        if AnsiContainsText(line,'duplicate unit') then exit;
        if AnsiContainsText(line,'is ignored for the current target platform') then exit;
        if AnsiContainsText(line,'Inlining disabled') then exit;
        if AnsiContainsText(line,'not yet supported inside inline procedure/function') then exit;
        if AnsiContainsText(line,'Check size of memory operand') then exit;
        if AnsiContainsText(line,'User defined: TODO') then exit;
        if AnsiContainsText(line,'Circular dependency detected when compiling target') then exit;
        if AnsiContainsText(line,'overriding recipe for target') then exit;
        if AnsiContainsText(line,'ignoring old recipe for target') then exit;
        if AnsiContainsText(line,'overriding commands for target') then exit;
        if AnsiContainsText(line,'ignoring old commands for target') then exit;
        if AnsiContainsText(line,'Case statement does not handle all possible cases') then exit;

        if AnsiContainsText(line,'(5059)') then exit; //function result not initialized

        if AnsiContainsText(line,'unreachable code') then exit;
        if AnsiContainsText(line,'Fix implicit pointer conversions') then exit;
        if AnsiContainsText(line,'are not related') then exit;
        if AnsiContainsText(line,'Constructor should be public') then exit;
        if AnsiContainsText(line,'is experimental') then exit;
        if AnsiContainsText(line,'This code is not thread-safe') then exit;

        if AnsiContainsText(line,'Range check error while') then exit;

        if AnsiContainsText(line,'linker'' input unused') then exit;

        // when generating help
        if AnsiContainsText(line,'is unknown') then exit;
        {$ifdef MSWINDOWS}
        if AnsiContainsText(line,'unable to determine the libgcc path') then exit;
        {$endif}

        if AnsiContainsText(line,'constant cast with potential data loss') then exit;

        if AnsiContainsText(line,'Removed non empty directory') then exit;
      end;

      // suppress "trivial"* build commands
      {$ifdef MSWINDOWS}
      if AnsiContainsText(line,'rm.exe ') then exit;
      if AnsiContainsText(line,'mkdir.exe ') then exit;
      if AnsiContainsText(line,'mv.exe ') then exit;
      if AnsiContainsText(line,'cmp.exe ') then exit;
      if (AnsiContainsText(line,'cp.exe ')) AND (AnsiContainsText(line,'.compiled')) then exit;
      //if AnsiContainsText(line,'ginstall.exe ') then exit;
      {$endif}

      s:='rm ';
      if AnsiStartsText(s,line) then exit;
      s:='rm -f ';
      if AnsiContainsText(line,'/'+s) OR AnsiStartsText(s,line) then exit;
      if AnsiContainsText(line,'/'+TrimRight(s)) OR AnsiStartsText(TrimRight(s),line) then exit;
      s:='rm -rf ';
      if AnsiContainsText(line,'/'+s) OR AnsiStartsText(s,line) then exit;
      if AnsiContainsText(line,'/'+TrimRight(s)) OR AnsiStartsText(TrimRight(s),line) then exit;
      s:='mkdir ';
      if AnsiContainsText(line,'/'+s) OR AnsiStartsText(s,line) then exit;
      s:='mv ';
      if AnsiContainsText(line,'/'+s) OR AnsiStartsText(s,line) then exit;
      s:='cp ';
      if ( (AnsiContainsText(line,'/'+s) OR AnsiStartsText(s,line)) AND (AnsiContainsText(line,'.compiled') OR AnsiContainsText(line,'.tmp')) ) then exit;
      s:='grep: ';
      if AnsiContainsText(line,'/'+s) OR AnsiStartsText(s,line) then exit;

      // remove hints and other "trivial"* warnings from output
      // these line are not that interesting for the average user of fpcupdeluxe !
      if AnsiContainsText(line,'assembling ') then exit;
      if AnsiContainsText(line,': entering directory ') then exit;
      if AnsiContainsText(line,': leaving directory ') then exit;
      // when generating help
      if AnsiContainsText(line,'illegal XML element: ') then exit;
      if AnsiContainsText(line,'Invalid paragraph content') then exit;
      if AnsiContainsText(line,'parsing used unit ') then exit;
      if AnsiContainsText(line,'extracting ') then exit;
      if AnsiContainsText(line,'directory not found for option') then exit;
      if AnsiContainsText(line,'hint(s) issued') then exit;
      if AnsiContainsText(line,'warning(s) issued') then exit;

      // during building of lazarus components, default compiler switches cause version and copyright info to be shown
      // do not know if this is allowed, but this version / copyright info is very redundant as it is shown everytime the compiler is called ...
      // I stand corrected if this has to be changed !
      if AnsiContainsText(line,'Copyright (c) 1993-') then exit;
      if AnsiContainsText(line,'Free Pascal Compiler version ') then exit;

      {$ifdef Darwin}
      s:='strip -no_uuid ';
      if AnsiContainsText(line,'/'+s) OR AnsiStartsText(s,line) then exit;
      if AnsiContainsText(line,'/'+TrimRight(s)) OR AnsiStartsText(TrimRight(s),line) then exit;
      s:='/usr/bin/codesign ';
      if AnsiStartsText(s,line) then exit;
      s:='/usr/bin/diff ';
      if AnsiStartsText(s,line) then exit;
      s:='svnversion: error: ';
      if AnsiStartsText(s,line) OR AnsiContainsText(line,s) then exit;
      {$endif}

      if AnsiContainsText(line,'is up to date.') then exit;
      if AnsiContainsText(line,'searching ') then exit;

      //Remove some Lazarus info
      if AnsiContainsText(line,'Info: (lazarus)') then exit;
      if AnsiStartsText('  File=',line) then exit;
      if AnsiStartsText('  State file=',line) then exit;
      if AnsiStartsText('### TCodeToolManager.HandleException:',line) then exit;

      //Remove some not so very interesting info
      if AnsiContainsText(line,'Writing Resource String Table') then exit;
      if AnsiContainsText(line,'Nothing to be done') then exit;

      // Some prehistoric FPC errors.
      if AnsiContainsText(line,'Unknown option.') then exit;

      // found modified files
      result:=true;
    end;
  end;
end;

procedure TExternalToolThread.SetTool(AValue: TExternalTool);
begin
  if FTool=AValue then Exit;
  if FTool<>nil then FTool.Thread:=nil;
  FTool:=AValue;
  if FTool<>nil then FTool.Thread:=Self;
end;

procedure TExternalToolThread.Execute;
type
  TErrorFrame = record
    Addr: Pointer;
    Line: shortstring;
  end;
  PErrorFrame = ^TErrorFrame;

var
  ErrorFrames: array[0..30] of TErrorFrame;
  ErrorFrameCount: integer;

  function GetExceptionStackTrace: string;
  var
    FrameCount: LongInt;
    Frames: PPointer;
    Cnt: LongInt;
    f: PErrorFrame;
    i: Integer;
  begin
    Result:='';
    FrameCount:=ExceptFrameCount;
    Frames:=ExceptFrames;
    ErrorFrames[0].Addr:=ExceptAddr;
    ErrorFrames[0].Line:='';
    ErrorFrameCount:=1;
    Cnt:=FrameCount;
    for i:=1 to Cnt do begin
      ErrorFrames[i].Addr:=Frames[i-1];
      ErrorFrames[i].Line:='';
      ErrorFrameCount:=i+1;
    end;
    for i:=0 to ErrorFrameCount-1 do begin
      f:=@ErrorFrames[i];
      try
        f^.Line:=copy(BackTraceStrFunc(f^.Addr),1,255);
      except
        f^.Line:=copy(SysBackTraceStr(f^.Addr),1,255);
      end;
    end;
    for i:=0 to ErrorFrameCount-1 do begin
      Result+=ErrorFrames[i].Line+LineEnding;
    end;
  end;

var
  Buf: string;

  function ReadInputPipe(aStream: TInputPipeStream; var LineBuf: string;
    IsStdErr: boolean): boolean;
  // true if some bytes have been read
  var
    Count: DWord;
    StartPos: Integer;
    i: DWord;
  begin
    Result:=false;
    if aStream=nil then exit;
    Count:=aStream.NumBytesAvailable;
    if Count=0 then exit;
    Count:=aStream.Read(Buf[1],Min(length(Buf),Count));
    if Count=0 then exit;
    Result:=true;
    StartPos:=1;
    i:=1;
    while i<=Count do
    begin
      if Buf[i] in [#10,#13] then
      begin
        LineBuf:=LineBuf+copy(Buf,StartPos,i-StartPos);
        if GetFilter(LineBuf,Tool.FFPCMagic) then
        begin
          if IsStdErr then
            fLines.AddObject(LineBuf,fLines)
          else
            fLines.Add(LineBuf);
        end;
        LineBuf:='';
        if (i<Count) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1])
        then
          inc(i);
        StartPos:=i+1;
      end;
      inc(i);
    end;
    LineBuf:=LineBuf+copy(Buf,StartPos,Count-StartPos+1);
  end;

const
  UpdateTimeDiff = 1000 div 10; // update 10 times a second, even if there is still work
var
  OutputLine, StdErrLine: String;
  LastUpdate: QWord;
  ErrMsg: String;
  ok: Boolean;
  HasOutput: Boolean;
  ProcessCounter:integer;
  aExit:longword;
begin
  SetLength({%H-}Buf,4096);

  //FillChar(Buf[1],SizeOf(Buf)-1,0);
  FillChar(ErrorFrames,SizeOf(ErrorFrames),0);

  ErrorFrameCount:=0;
  ProcessCounter:=0;

  fLines:=TStringList.Create;
  try
    try
      if Tool.Stage<>etsStarting then exit;

      if not FileIsExecutable(Tool.Process.Executable) then
      begin
        Tool.ErrorMessage:=Format(lisCanNotExecute, [Tool.Process.Executable]);
        Tool.ProcessStopped;
        exit;
      end;
      if not DirectoryExists(ChompPathDelim(Tool.Process.CurrentDirectory)) then
      begin
        Tool.ErrorMessage:=Format(lisMissingDirectory, [Tool.Process.
          CurrentDirectory]);
        Tool.ProcessStopped;
        exit;
      end;

      ok:=false;
      try
        Tool.Process.PipeBufferSize:=Max(Tool.Process.PipeBufferSize,64*1024);
        Tool.Process.Execute;
        ok:=true;
      except
        on E: Exception do
        begin
          if Tool.ErrorMessage='' then
            Tool.ErrorMessage:=Format(lisUnableToExecute, [E.Message]);
        end;
      end;
      if (not ok) then
      begin
        Tool.ProcessStopped;
        exit;
      end;
      if Tool.Stage>=etsStopped then exit;

      Tool.ProcessRunning;

      if Tool.Stage>=etsStopped then exit;

      OutputLine:='';
      StdErrLine:='';
      LastUpdate:=GetTickCount64;
      while (Tool<>nil) and (Tool.Stage=etsRunning) do
      begin
        if Tool.ReadStdOutBeforeErr then begin
          HasOutput:=ReadInputPipe(Tool.Process.Output,OutputLine,false)
                  or ReadInputPipe(Tool.Process.Stderr,StdErrLine,true);
        end else begin
          HasOutput:=ReadInputPipe(Tool.Process.Stderr,StdErrLine,true)
                  or ReadInputPipe(Tool.Process.Output,OutputLine,false);
        end;
        if (not HasOutput) then
        begin
          if not Tool.Process.Running then break;
        end;
        if (fLines.Count>0)
        and (Abs(int64(GetTickCount64)-LastUpdate)>UpdateTimeDiff) then
        begin
          Tool.AddOutputLines(fLines);
          fLines.Clear;
          LastUpdate:=GetTickCount64;
        end;

        if Assigned(Tool.OnUpdateEvent) then Tool.OnUpdateEvent(self,Tool.Stage);

        if (not HasOutput) then
          sleep(50);
        //else
        //  sleep(0);// allow context swith
      end;
      // add rest of output

      if (OutputLine<>'') then fLines.Add(OutputLine);
      if (StdErrLine<>'') then fLines.Add(StdErrLine);

      if (Tool<>nil) then
      begin
        if (fLines.Count>0) then
        begin
          Tool.AddOutputLines(fLines);
          fLines.Clear;
        end;
        if Assigned(Tool.OnUpdateEvent) then Tool.OnUpdateEvent(self,Tool.Stage);
      end;

      try
        if Tool.Stage>=etsStopped then exit;
        Tool.ExitStatus:=Tool.Process.ExitStatus;
        Tool.ExitCode:=Tool.Process.ExitCode;
      except
        Tool.ErrorMessage:=lisUnableToReadProcessExitStatus;
      end;
    except
      on E: Exception do begin
        if (Tool<>nil) and (Tool.ErrorMessage='') then
        begin
          Tool.ErrorMessage:=E.Message;
          ErrMsg:=GetExceptionStackTrace;
          Tool.ErrorMessage:=E.Message+LineEnding+ErrMsg;
        end;
      end;
    end;
  finally
    // clean up
    try
      Finalize(buf);
      FreeAndNil(fLines);
    except
      on E: Exception do
      begin
        if Tool<>nil then
          Tool.ErrorMessage:=Format(lisFreeingBufferLines, [E.Message]);
      end;
    end;
  end;
  if Tool.Stage>=etsStopped then exit;
  if Tool<>nil then Tool.ProcessStopped;
end;

destructor TExternalToolThread.Destroy;
begin
  FTool:=nil;
  inherited Destroy;
end;

procedure ThreadLog(const aMsg: string;const aEvent:TEventType);
{$ifdef LCL}
const
  WM_THREADINFO = LM_USER + 2010;
var
  aMessage:string;
  PInfo: PChar;
begin
  if (Length(aMsg)>0) then
  begin
    {
    if aEvent<>etCustom then
      aMessage:=BeginSnippet+' '+Seriousness[aEvent]+' '+ aMsg
    else
      aMessage:=BeginSnippet+' '+aMsg;
    }
    if aEvent=etError then
      aMessage:=BeginSnippet+' '+'ERROR: '+aMsg
    else
    if aEvent=etWarning then
      aMessage:=BeginSnippet+' '+'WARNING: '+aMsg
    else
    if aEvent=etCustom then
      aMessage:=BeginSnippet+' '+aMsg
    else
      aMessage:=aMsg;
  end else aMessage:='';

  PInfo := StrAlloc(Length(aMessage)+16);
  StrPCopy(PInfo, aMessage);
  if (Assigned(Application) AND Assigned(Application.MainForm)) then
  begin
    if not PostMessage(Application.MainForm.Handle, WM_THREADINFO, 0, {%H-}LPARAM(PInfo)) then
      StrDispose(PInfo);
  end;
end;
{$else}
begin
  if aEvent=etError then write(BeginSnippet+' '+'ERROR: ');
  if aEvent=etWarning then write(BeginSnippet+' '+'WARNING: ');
  if aEvent=etCustom then write(BeginSnippet+' ');
  writeln(aMsg);
end;
{$endif}

end.

