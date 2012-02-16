unit processutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process;
type

  TDumpFunc = procedure (Sender:TObject; output:string);
  { TProcessEnvironment }

  TProcessEnvironment = class(TObject)
    private
      FEnvironmentList:TStringList;
      FCaseSensitive:boolean;
      function GetVarIndex(VarName:string):integer;
    public
      function GetVar(VarName:string):string;
      procedure SetVar(VarName,VarValue:string);
      property EnvironmentList:TStringList read FEnvironmentList;
      constructor Create;
      destructor Destroy; override;
    end;

  { TProcessEx }

  TProcessEx = class(TProcess)
    private
      FCmdLine: string;
      FExceptionInfoStrings: TstringList;
      FExecutable: string;
      FExitStatus: integer;
      FOnOutput: TDumpFunc;
      FOutputStrings: TstringList;
      FOutStream: TMemoryStream;
      FProcess: TProcess;
      FProcessEnvironment:TProcessEnvironment;
      function GetExceptionInfo: string;
      function GetOutputString: string;
      function GetOutputStrings: TstringList;
      function GetParametersString: String;
      function GetProcessEnvironment: TProcessEnvironment;
      procedure SetOnOutput(AValue: TDumpFunc);
      procedure SetParametersString(AValue: String);
    public
      procedure Execute;
      property Environment:TProcessEnvironment read GetProcessEnvironment;
      property ExceptionInfo:string read GetExceptionInfo;
      property ExceptionInfoStrings:TstringList read FExceptionInfoStrings;
      property ExitStatus:integer read FExitStatus;
      property OnOutput:TDumpFunc read FOnOutput write SetOnOutput;
      property OutputString:string read GetOutputString;
      property OutputStrings:TstringList read GetOutputStrings;
      property ParametersString:String read GetParametersString write SetParametersString;
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
    end;

// Convenience functions

function ExecuteCommandHidden(const Executable, Parameters: string; Verbose:boolean): integer; overload;
function ExecuteCommandHidden(const Executable, Parameters: string; var Output:string; Verbose:boolean): integer; overload;



implementation

{ TProcessEx }

function TProcessEx.GetOutputString: string;
begin
  result:=OutputStrings.Text;
end;

function TProcessEx.GetOutputStrings: TstringList;
begin
  if (FOutputStrings.Count=0) and (FOutStream.Size>0) then
    begin
    FOutStream.Position := 0;
    FOutputStrings.LoadFromStream(FOutStream);
    end;
  result:=FOutputStrings;
end;

function TProcessEx.GetParametersString: String;
begin
  result:=Parameters.text;
end;

function TProcessEx.GetExceptionInfo: string;
begin
  result:=FExceptionInfoStrings.Text;
end;

function TProcessEx.GetProcessEnvironment: TProcessEnvironment;
begin
  If not assigned(FProcessEnvironment) then
    FProcessEnvironment:=TProcessEnvironment.Create;
end;

procedure TProcessEx.SetOnOutput(AValue: TDumpFunc);
begin
  if FOnOutput=AValue then Exit;
  FOnOutput:=AValue;
end;

procedure TProcessEx.SetParametersString(AValue: String);
begin
  CommandToList(AValue,Parameters);
end;

procedure TProcessEx.Execute;

  function ReadOutput: boolean;

  const
    BufSize = 4096;
  var
    Buffer: array[0..BufSize - 1] of byte;
    ReadBytes: integer;
  begin
    Result := False;
    while Output.NumBytesAvailable > 0 do
    begin
      ReadBytes := Output.Read(Buffer, BufSize);
      FOutStream.Write(Buffer, ReadBytes);
      if Assigned(FOnOutput) then
        FOnOutput(Self,copy(pchar(@buffer[0]),1,ReadBytes));
      Result := True;
    end;
  end;

begin
  try
    // "Normal" linux and DOS exit codes are in the range 0 to 255.
    // Windows System Error Codes are 0 to 15999
    // Use negatives for internal errors.
    FExitStatus:=-1;
    FExceptionInfoStrings.Clear;
    FOutputStrings.Clear;
    if Assigned(FProcessEnvironment) then
      inherited Environment:=FProcessEnvironment.EnvironmentList;
    Options := Options +[poUsePipes, poStderrToOutPut];
    inherited Execute;
    while Running do
    begin
      if not ReadOutput then
        Sleep(50);
    end;
    ReadOutput;
    FExitStatus:=inherited ExitStatus;
  except
    on E: Exception do
    begin
    FExceptionInfoStrings.Add('Exception calling '+Executable+' '+Parameters.Text);
    FExceptionInfoStrings.Add('Details: '+E.ClassName+'/'+E.Message);
    FExitStatus:=-2;
    end;
  end;
end;

constructor TProcessEx.Create(AOwner : TComponent);
begin
  inherited;
  FExceptionInfoStrings:= TstringList.Create;
  FOutputStrings:= TstringList.Create;
  FOutStream := TMemoryStream.Create;
end;

destructor TProcessEx.Destroy;
begin
  FExceptionInfoStrings.Free;
  FOutputStrings.Free;
  FOutStream.Free;
  If assigned(FProcessEnvironment) then
    FProcessEnvironment.Free;
  inherited Destroy;
end;

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
  if idx>0 then
    result:=ExtractVal(FEnvironmentList[idx])
  else
    result:='';
end;

procedure TProcessEnvironment.SetVar(VarName, VarValue: string);
var
  idx:integer;
  s:string;
begin
  idx:=GetVarIndex(VarName);
  s:=trim(Varname)+'='+trim(VarValue);
  if idx>0 then
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


procedure DumpConsole(Sender:TObject; output:string);
begin
  writeln(output);
end;

function ExecuteCommandHidden(const Executable, Parameters: string; Verbose:boolean): integer;
var
  s:string;
begin
  Result:=ExecuteCommandHidden(Executable, Parameters,s,Verbose);
end;

function ExecuteCommandHidden(const Executable, Parameters: string; var Output: string
  ; Verbose:boolean): integer;
var
  PE:TProcessEx;
begin
  PE:=TProcessEx.Create(nil);
  try
    PE.Executable:=Executable;
    PE.ParametersString:=Parameters;
    PE.ShowWindow := swoHIDE;
    if Verbose then
      PE.OnOutput:=@DumpConsole;
    PE.Execute;
    Output:=PE.OutputString;
    Result:=PE.ExitStatus;
  finally
    PE.Free;
  end;
end;

end.

