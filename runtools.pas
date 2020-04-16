unit RunTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$ifdef LCL}
  LMessages,
  {$endif}
  UTF8Process;

{$ifdef LCL}
const
  WM_THREADINFO = LM_USER + 2010;
{$endif}

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

  { TAbstractExternalTool
    Implemented by the IDE.
    Create one with ExternalToolList.Add or AddDummy.
    Access needs Tool.Enter/LeaveCriticalSection. }

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
    function GetCmdLineParams: string;
    procedure SetCmdLineParams(aParams: string);
    procedure SetTitle(const AValue: string);
  protected
    FErrorMessage: string;
    FTerminated: boolean;
    FStage: TExternalToolStage;
    FWorkerOutput: TStringList;
    FProcess: TProcessUTF8;
    function GetProcessEnvironment: TProcessEnvironment;
    procedure DoCallNotifyHandler(HandlerType: TExternalToolHandler);
    procedure DoExecute; virtual; abstract;  // starts thread, returns immediately
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    function CanFree: boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EnterCriticalSection;// virtual; // always use before access, when using Tool and View: always lock Tool before View
    procedure LeaveCriticalSection;// virtual;
    procedure AutoFree; // (only main thread) free if not in use

    property Title: string read FTitle write SetTitle;
    property Data: TObject read FData write FData; // free for user, e.g. the IDE uses TIDEExternalToolData
    property FreeData: boolean read FFreeData write FFreeData default false; // true = auto free Data on destroy

    // process
    property Process: TProcessUTF8 read FProcess;
    property CmdLineParams: string read GetCmdLineParams write SetCmdLineParams;
    property Stage: TExternalToolStage read FStage;
    procedure Execute; virtual; abstract;
    procedure Terminate; virtual; abstract;
    procedure WaitForExit; virtual; abstract;
    property Terminated: boolean read FTerminated;
    property ExitCode: integer read FExitCode write FExitCode;
    property ExitStatus: integer read FExitStatus write FExitStatus;
    property ErrorMessage: string read FErrorMessage write FErrorMessage; // error executing tool
    property ReadStdOutBeforeErr: boolean read FReadStdOutBeforeErr write FReadStdOutBeforeErr;
    property Environment:TProcessEnvironment read GetProcessEnvironment;

    // output
    property WorkerOutput: TStringList read FWorkerOutput; // the raw output
  end;

  TExternalTool = class;

  { TExternalToolThread }

  TExternalToolThread = class(TThread)
  private
    fLines: TStringList;
    FTool: TExternalTool;
    procedure SetTool(AValue: TExternalTool);
  public
    property Tool: TExternalTool read FTool write SetTool;
    procedure Execute; override;
    destructor Destroy; override;
  end;

  { TExternalTool }

  TExternalTool = class(TAbstractExternalTool)
  private
    FThread: TExternalToolThread;
    procedure ProcessRunning; // (worker thread) after Process.Execute
    procedure ProcessStopped; // (worker thread) when process stopped
    procedure AddOutputLines(Lines: TStringList); // (worker thread) when new output arrived
    procedure SetThread(AValue: TExternalToolThread); // main or worker thread
    procedure DoTerminate; // (main thread)
  protected
    procedure DoExecute; override;           // (main thread)
    procedure DoStart;                       // (main thread)
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CanFree: boolean; override;
    procedure QueueAsyncAutoFree; virtual; abstract;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property Thread: TExternalToolThread read FThread write SetThread;
    procedure Execute; override;
    procedure Terminate; override;
    procedure WaitForExit; override;
    function GetExeInfo:string;
    function CanStart: boolean;
  end;

  TExternalToolClass = class of TExternalTool;

implementation

uses
  {$ifdef LCL}
  Forms,Controls,LCLIntf,
  {$endif}
  process,
  Pipes,
  Math,
  LazUTF8,
  FileUtil,
  LazFileUtils;

{$ifdef LCL}
procedure ThreadLog(Msg: string);
var
  PInfo: PChar;
begin
  PInfo := StrAlloc(Length(Msg)+1);
  StrCopy(PInfo, PChar(Msg));
  Application.MainForm.Handle;
  PostMessage(Application.MainForm.Handle, WM_THREADINFO, NativeUInt(PInfo), 0);
end;
{$endif}


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
  if (Length(VarName)=0) OR (Length(VarValue)=0) then exit;
  idx:=GetVarIndex(VarName);
  s:=trim(Varname)+'='+trim(VarValue);
  if idx>=0 then
    FEnvironmentList[idx]:=s
  else
    FEnvironmentList.Add(s);
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
    EnvironmentList.Add(trim(GetEnvironmentString(i)));
end;

destructor TProcessEnvironment.Destroy;
begin
  FEnvironmentList.Free;
  inherited Destroy;
end;

{ TAbstractExternalTool }

function TAbstractExternalTool.GetCmdLineParams: string;
begin
  Result:=MergeCmdLineParams(Process.Parameters);
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

procedure TAbstractExternalTool.SetTitle(const AValue: string);
begin
  if FTitle=AValue then exit;
  FTitle:=AValue;
end;

procedure TAbstractExternalTool.DoCallNotifyHandler(
  HandlerType: TExternalToolHandler);
begin
  //FHandlers[HandlerType].CallNotifyEvents(Self);
end;


procedure TAbstractExternalTool.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    EnterCriticalSection;
    try
    finally
      LeaveCriticalSection;
    end;
  end;
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
    FStage:=etsStopped;
  finally
    LeaveCriticalSection;
  end;
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

    // feed new lines into all parsers, converting raw lines into messages
    for Line:=OldOutputCount to WorkerOutput.Count-1 do begin
      LineStr:=WorkerOutput[Line];
      {$ifdef LCL}
      ThreadLog(LineStr);
      {$endif}
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
  if CallAutoFree then begin
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
  FProcess:=TProcessUTF8.Create(nil);
  FProcess.Options:= [poUsePipes{$IFDEF Windows},poStderrToOutPut{$ENDIF}];
  FProcess.ShowWindow := swoHide;
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

  // set Stage to etsInitializing
  EnterCriticalSection;
  try
    if Stage<>etsInit then
      raise Exception.Create('TExternalTool.Execute: already initialized');
    FStage:=etsInitializing;
  finally
    LeaveCriticalSection;
  end;

  // init CurrentDirectory
  Process.CurrentDirectory:=TrimFilename(Process.CurrentDirectory);
  if not FilenameIsAbsolute(Process.CurrentDirectory) then
    Process.CurrentDirectory:=AppendPathDelim(GetCurrentDirUTF8)+Process.CurrentDirectory;

  // init Executable
  Process.Executable:=TrimFilename(Process.Executable);
  if not FilenameIsAbsolute(Process.Executable) then begin
    if ExtractFilePath(Process.Executable)<>'' then
      Process.Executable:=AppendPathDelim(GetCurrentDirUTF8)+Process.Executable
    else if Process.Executable='' then begin
      ErrorMessage:=Format(lisToolHasNoExecutable, [Title]);
      CheckError;
      exit;
    end else begin
      ExeFile:=FindDefaultExecutablePath(Process.Executable,GetCurrentDirUTF8);
      if ExeFile='' then begin
        ErrorMessage:=Format(lisCanNotFindExecutable, [Process.Executable]);
        CheckError;
        exit;
      end;
      Process.Executable:=ExeFile;
    end;
  end;
  ExeFile:=Process.Executable;
  if not FileExistsUTF8(ExeFile) then begin
    ErrorMessage:=Format(lisMissingExecutable, [ExeFile]);
    CheckError;
    exit;
  end;
  if DirectoryExistsUTF8(ExeFile) then begin
    ErrorMessage:=Format(lisExecutableIsADirectory, [ExeFile]);
    CheckError;
    exit;
  end;
  if not FileIsExecutable(ExeFile) then begin
    ErrorMessage:=Format(lisExecutableLacksThePermissionToRun, [ExeFile]);
    CheckError;
    exit;
  end;

  // init misc
  if Assigned(FProcessEnvironment) then
      Process.Environment:=FProcessEnvironment.EnvironmentList;

  // set Stage to etsWaitingForStart
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
  // set Stage to etsStarting
  EnterCriticalSection;
  try
    if Stage<>etsWaitingForStart then
      raise Exception.Create('TExternalTool.Execute: already started');
    FStage:=etsStarting;
  finally
    LeaveCriticalSection;
  end;

  // start thread
  if Thread=nil then begin
    FThread:=TExternalToolThread.Create(true);
    Thread.Tool:=Self;
    FThread.FreeOnTerminate:=true;
  end;
  Thread.Start;
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
  end;
end;

procedure TExternalTool.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
  end;
end;

function TExternalTool.CanFree: boolean;
begin
  Result:=(FThread=nil)
       and inherited CanFree;
end;

{
procedure TExternalTool.SyncAutoFree(aData: PtrInt);
begin
  AutoFree;
end;

procedure TExternalTool.QueueAsyncAutoFree;
begin
  Application.QueueAsyncCall(@SyncAutoFree,0);
end;
}

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
    raise Exception.Create('TExternalTool.Execute "'+Title+'" already started');
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
    EnterCriticalSection;
    try
      if Stage=etsDestroying then exit;
      if (Stage=etsStopped) then
      begin
        //Make ready for next invoke in case of re-use.
        FStage:=etsInit;
        exit;
      end;
    finally
      LeaveCriticalSection;
    end;
    // call synchronized tasks, this might free this tool
    if MainThreadID=ThreadID then
    begin
      {$ifdef LCL}
      try
        Application.ProcessMessages;
      except
        Application.HandleException(Application);
      end;
      {$endif}
    end;
    // still running => wait
    Sleep(10);
  until false;
end;

function TExternalTool.GetExeInfo:string;
begin
  result:='Executing: '+Process.Executable+'. With params: '+CmdLineParams+' (working dir: '+ Process.CurrentDirectory +')';
end;

{ TExternalToolThread }

procedure TExternalToolThread.SetTool(AValue: TExternalTool);
begin
  if FTool=AValue then Exit;
  if FTool<>nil then
    FTool.Thread:=nil;
  FTool:=AValue;
  if FTool<>nil then
    FTool.Thread:=Self;
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
    while i<=Count do begin
      if Buf[i] in [#10,#13] then begin
        LineBuf:=LineBuf+copy(Buf,StartPos,i-StartPos);
        if IsStdErr then
          fLines.AddObject(LineBuf,fLines)
        else
          fLines.Add(LineBuf);
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
  UpdateTimeDiff = 1000 div 5; // update five times a second, even if there is still work
var
  OutputLine, StdErrLine: String;
  LastUpdate: QWord;
  ErrMsg: String;
  ok: Boolean;
  HasOutput: Boolean;
begin
  SetLength({%H-}Buf,4096);
  ErrorFrameCount:=0;
  fLines:=TStringList.Create;
  try
    try
      if Tool.Stage<>etsStarting then begin
        exit;
      end;

      if not FileIsExecutable(Tool.Process.Executable) then begin
        Tool.ErrorMessage:=Format(lisCanNotExecute, [Tool.Process.Executable]);
        Tool.ProcessStopped;
        exit;
      end;
      if not DirectoryExistsUTF8(ChompPathDelim(Tool.Process.CurrentDirectory))
      then begin
        Tool.ErrorMessage:=Format(lisMissingDirectory, [Tool.Process.
          CurrentDirectory]);
        Tool.ProcessStopped;
        exit;
      end;

      // Under Unix TProcess uses fpFork, which means the current thread is
      // duplicated. One is the old thread and one runs fpExecve.
      // If fpExecve runs, then it will not return.
      // If fpExecve fails it returns via an exception and this thread runs twice.
      ok:=false;
      try
        // now execute
        Tool.Process.PipeBufferSize:=Max(Tool.Process.PipeBufferSize,64*1024);
        Tool.Process.Execute;
        ok:=true;
      except
        on E: Exception do begin
          // BEWARE: we are now either in the normal thread or in the failed forked thread
          if Tool.ErrorMessage='' then
            Tool.ErrorMessage:=Format(lisUnableToExecute, [E.Message]);
        end;
      end;
      // BEWARE: we are now either in the normal thread or in the failed forked thread
      if not ok then begin
        Tool.ProcessStopped;
        exit;
      end;
      // we are now in the normal thread
      if Tool.Stage>=etsStopped then
        exit;
      Tool.ProcessRunning;
      if Tool.Stage>=etsStopped then
        exit;

      OutputLine:='';
      StdErrLine:='';
      LastUpdate:=GetTickCount64;
      while (Tool<>nil) and (Tool.Stage=etsRunning) do begin
        if Tool.ReadStdOutBeforeErr then begin
          HasOutput:=ReadInputPipe(Tool.Process.Output,OutputLine,false)
                  or ReadInputPipe(Tool.Process.Stderr,StdErrLine,true);
        end else begin
          HasOutput:=ReadInputPipe(Tool.Process.Stderr,StdErrLine,true)
                  or ReadInputPipe(Tool.Process.Output,OutputLine,false);
        end;
        if (not HasOutput) then begin
          // no more pending output
          if not Tool.Process.Running then break;
        end;
        if (fLines.Count>0)
        and (Abs(int64(GetTickCount64)-LastUpdate)>UpdateTimeDiff) then begin
          Tool.AddOutputLines(fLines);
          fLines.Clear;
          LastUpdate:=GetTickCount64;
        end;
        if (not HasOutput) then begin
          // no more pending output and process is still running
          // => tool needs some time
          Sleep(50);
        end;
      end;
      // add rest of output
      if (OutputLine<>'') then
        fLines.Add(OutputLine);
      if (StdErrLine<>'') then
        fLines.Add(StdErrLine);
      if (Tool<>nil) and (fLines.Count>0) then begin
        Tool.AddOutputLines(fLines);
        fLines.Clear;
      end;
      try
        if Tool.Stage>=etsStopped then begin
          exit;
        end;
        Tool.ExitStatus:=Tool.Process.ExitStatus;
        Tool.ExitCode:=Tool.Process.ExitCode;
      except
        Tool.ErrorMessage:=lisUnableToReadProcessExitStatus;
      end;
    except
      on E: Exception do begin
        if (Tool<>nil) and (Tool.ErrorMessage='') then begin
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
      on E: Exception do begin
        if Tool<>nil then
          Tool.ErrorMessage:=Format(lisFreeingBufferLines, [E.Message]);
      end;
    end;
  end;
  if Tool.Stage>=etsStopped then begin
    exit;
  end;
  if Tool<>nil then
    Tool.ProcessStopped;
end;

destructor TExternalToolThread.Destroy;
begin
  FTool:=nil;
  inherited Destroy;
end;

end.

