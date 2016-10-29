unit fpftpclient;

interface

{$MODE Delphi}

uses
  Classes,
  SysUtils,
  //Generics.Defaults,Generics.Collections,
  strutils, dateutils,
  ssockets;

type

  //TInetSocket = TSocket;

  TRemoteFindData = record
    Path: string;
    Name: string;
    Date: TDateTime;
    Size: QWord;
  end;

{ TTextEvent where Text is a value being processed }

  TTextEvent = procedure(Sender: TObject; const Text: string) of object;

  TFtpClient = class(TObject)
  private
    FCommand: TInetSocket;
    FConnected:boolean;
    FHost: string;
    FPort: Word;
    FUserName: string;
    FPassword: string;
    FTransfering: Boolean;
    FFindList: TStringArray;
    FFindIndex: Integer;
    FOnCommand: TTextEvent;
    FOnResponse: TTextEvent;

    type
      TResponse = object
      public
        Valid: Boolean;
        Raw: string;
        Code: Integer;
        Message: string;
        function IsFail(Low, High: Integer): Boolean;
        function IsPass(Low, High: Integer): Boolean;
      end;

    function FileModeBinary: Boolean;
    function Passive(out Socket: TInetSocket): Boolean;
    procedure Send(const S: string; out R: TResponse);
    procedure Recv(out R: TResponse);
    procedure SetConnected(Value: Boolean);
    function GetConnected: Boolean;
  protected
    { Invoke the OnProgress event }
    //procedure DoProgress(const Size, Transmitted: QWord); virtual;
  public
    { Create a new file transfer object }
    constructor Create;
    destructor Destroy; override;
    { Attempt to open a file transfer connection using the host, port, username, and password }
    function Connect: Boolean;
    { Close any opened connection }
    procedure Disconnect;
    { Cancel any ongoing transfers }
    procedure Cancel;
    { Returns the current remote directory }
    function GetCurrentDir: string;
    { Returns true if a remote directory exists }
    function DirExists(const Dir: string): Boolean;
    { Change to a new current remote directory }
    function ChangeDir(const Dir: string): Boolean;
    { Create a new remote directory }
    function MakeDir(const Dir: string): Boolean;
    { Delete an existing remote directory }
    function RemoveDir(const Dir: string): Boolean;
    { Delete an existing remote file }
    function FileDelete(const FileName: string): Boolean;
    { Returns true if a remote file exists }
    function FileExists(const FileName: string): Boolean;
    { Rename a remote file, works with directories too }
    function FileRename(const OldName, NewName: string): Boolean;
    { Retrieve the size of a remote file }
    function FileSize(const FileName: string): QWord;
    { Retrieve the modified date of a remote file }
    function FileDate(const FileName: string): TDateTime;
    { Initiate an file upload to the remote server }
    function FilePut(const LocalFile, RemoteFile: string; Overwrite: Boolean = True): Boolean;
    { Request a file download from the remote server }
    function FileGet(const RemoteFile, LocalFile: string; Overwrite: Boolean = True): Boolean;
    { Retrieve a text mode listing files and folders }
    function FileList(const Path: string = ''): string;
    { Initiate a structured listing files and folders with an optional attribute mask }
    function FindFirst(const Path: string; out FindData: TRemoteFindData):Boolean;
    { Continue with the next listing started by FindFirst }
    function FindNext(out FindData: TRemoteFindData): Boolean;
    { Returns true when connected to a remote server, otherwise acts like connect and disconnect }
    property Connected: Boolean read GetConnected write SetConnected;
    { The name of the host to resolve when connecting }
    property Host: string read FHost write FHost;
    { The port used for issuing ftp commands, defaults to 21 }
    property Port: Word read FPort write FPort;
    { The username used when connecting, defaults to anonymous }
    property UserName: string read FUserName write FUserName;
    { The password used when connecting, defaults to an email address }
    property Password: string read FPassword write FPassword;
    { An event invoked echoing ftp commands issued by the client }
    property OnCommand: TTextEvent read FOnCommand write FOnCommand;
    { An event invoked when responses are read from the remote server }
    property OnResponse: TTextEvent read FOnResponse write FOnResponse;
    { An event continuously invoked as file transfers occur }
    //property OnProgress: TTransmitEvent read FOnProgress write FOnProgress;
  end;

implementation

{ TResponse }

function TFtpClient.TResponse.IsFail(Low, High: Integer): Boolean;
begin
  if not Valid then
    Result := True
  else
    Result := ((Code>=Low) AND (Code<=High));
end;

function TFtpClient.TResponse.IsPass(Low, High: Integer): Boolean;
begin
  Result := Valid and ((Code>=Low) AND (Code<=High));
end;

{ TFtpClient }

constructor TFtpClient.Create;
begin
  inherited Create;
  FHost := 'localhost';
  FUserName := 'anonymous';
  FPassword := 'user@email.com';
  FPort := 21;
  FConnected := False;
  FCommand := nil;
end;

destructor TFtpClient.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TFtpClient.Send(const S: string; out R: TResponse);
begin
  R.Valid := False;
  R.Code := 0;
  R.Message := '';
  R.Raw := '';
  if Assigned(FOnCommand) then
    FOnCommand(Self, S);
  FCommand.WriteAnsiString(S + #13#10);
  Recv(R);
end;

function CheckTerminated(S: string): Boolean;
var
  SA:TStringArray;
  i:integer;
begin
  if S.EndsWith(#10) then
  begin
    SA := S.Trim.Split(#10);
    S  := SA[High(SA)];
    SA := S.Split(' ');
    S  := SA[low(SA)];
    i  := StrToIntDef(S, 0);
    Result := (S.Length = 3) and (i>=100) and (i<=600);
    Finalize(SA);
  end;
end;

procedure TFtpClient.Recv(out R: TResponse);
var
  Text, S: string;
  SA:TStringArray;
  I: Integer;
begin
  S:='';
  R.Valid := False;
  R.Code := 0;
  R.Message := '';
  R.Raw := '';
  //if not FCommand.Connected then
  //  Exit;
  Text := '';
  while FCommand.Read(S, 3000) > 0 do
  begin
    Text := Text + S;
    if CheckTerminated(Text) then
      Break;
  end;
  if Assigned(FOnResponse) then
    FOnResponse(Self, Text);
  R.Raw := Text;
  R.Message := AdjustLineBreaks(R.Raw.Trim,tlbsCRLF);
  SA := R.Message.Split(#13#10);
  R.Message := SA[high(SA)];
  if length(SA)>1 then
  begin
    SetLength(SA,length(SA)-1);

    I:=Pos(' ',R.Message);
    if I > 0 then
       if I = 1 then
          S := ''
        else
          S := Copy(R.Message, 1, I - 1)
      else
        S := R.Message;
    R.Code := StrToIntDef(S, 0);

    R.Valid := R.Code > 0;
    if R.Valid then
    begin
      I:=Pos(' ',R.Message);
      if I > 0
        then S := Copy(R.Message, I + 1)
        else S := '';
      R.Message := S.Trim;
    end else R.Message := '';

  end else R.Message := '';
  Finalize(SA);
end;

function TFtpClient.Connect: Boolean;
var
  R: TResponse;
begin
  Result := False;
  Disconnect;
  if (IsEmptyStr(FHost,[' '])) or (FPort = 0) then
  //if (IsEmptyStr(FHost,[' '])) or (IsEmptyStr(FUserName,[' '])) or (IsEmptyStr(FPassword,[' '])) or (FPort = 0) then
    Exit;
  try
    FCommand := TInetSocket.Create(FHost, FPort);
  except
    FCommand := nil;
  end;
  if (FCommand = nil) then exit;

  FCommand.Connect;
  Recv(R);
  if R.IsFail(200, 299) then
  begin
    Disconnect;
    Exit;
  end;
  Send('USER ' + FUserName, R);
  if R.IsFail(200, 399) then
  begin
    Disconnect;
    Exit;
  end;
  Send('PASS ' + FPassword, R);
  if R.IsFail(200, 299) then
  begin
    Disconnect;
    Exit;
  end;
  FConnected:=True;
  Result := True;
end;

procedure TFtpClient.Disconnect;
var
  R: TResponse;
begin
  if Assigned(FCommand) then
  begin
    Cancel;
    Send('QUIT', R);
    FCommand.Destroy;
    FCommand   := nil;
  end;
  FConnected := False;
end;

procedure TFtpClient.Cancel;
var
  R: TResponse;
begin
  if FTransfering then
  begin
    Send('ABOR', R);
    FTransfering := False;
  end;
end;

function TFtpClient.GetCurrentDir: string;
var
  R: TResponse;
  I: integer;
  S: string;
begin
  Send('PWD', R);
  if R.IsPass(200, 299) then
  begin
    if R.Message.Contains('"') then
      Result := R.Message.DeQuotedString('"')
    else if R.Message.Contains('''') then
      Result := R.Message.DeQuotedString('''')
    else
    begin
      R.Message := R.Message.Trim;
      I:=Pos(' ',R.Message);
      if I > 0 then
         if I = 1 then
            S := ''
          else
            S := Copy(R.Message, 1, I - 1)
        else
          S := R.Message;
      Result := S;
    end;
  end
  else
    Result := '';
end;

function TFtpClient.DirExists(const Dir: string): Boolean;
var
  R: TResponse;
  S: string;
begin
  Result := False;
  S := GetCurrentDir;
  if S = '' then
    Exit;
  Send('CWD ' + Dir.QuotedString, R);
  if R.IsPass(200, 299) then
  begin
    Result := True;
    Send('CWD ' + S.QuotedString, R);
  end;
end;

function TFtpClient.ChangeDir(const Dir: string): Boolean;
var
  R: TResponse;
begin
  Send('CWD ' + Dir.QuotedString, R);
  Result := R.IsPass(200, 299);
end;

function TFtpClient.MakeDir(const Dir: string): Boolean;
var
  R: TResponse;
begin
  Send('MKD ' + Dir.QuotedString, R);
  Result := R.IsPass(200, 299);
end;

function TFtpClient.RemoveDir(const Dir: string): Boolean;
var
  R: TResponse;
begin
  Send('RMD ' + Dir.QuotedString, R);
  Result := R.IsPass(200, 299);
end;

function TFtpClient.FileDelete(const FileName: string): Boolean;
var
  R: TResponse;
begin
  Send('DELE ' + FileName.QuotedString, R);
  Result := R.IsPass(200, 299);
end;

function TFtpClient.FileExists(const FileName: string): Boolean;
var
  R: TResponse;
begin
  Send('SIZE ' + FileName.QuotedString, R);
  Result := R.IsPass(200, 299);
end;

function TFtpClient.FileRename(const OldName, NewName: string): Boolean;
var
  R: TResponse;
begin
  Send('RNFR ' + OldName.QuotedString, R);
  if R.IsPass(200, 299) then
  begin
    Send('RNTO ' + NewName.QuotedString, R);
    Result := R.IsPass(200, 299);
  end
  else
    Result := False;
end;

function TFtpClient.FileSize(const FileName: string): QWord;
var
  R: TResponse;
begin
  Send('SIZE ' + FileName.QuotedString, R);
  if R.IsPass(200, 299) then
    Result := StrToQWordDef(R.Message, 0)
  else
    Result := 0;
end;

function TFtpClient.FileDate(const FileName: string): TDateTime;
var
  R: TResponse;
  S: string;
  Year, Month, Day, Hour, Minute, Second: Word;
begin
  Result := 0;
  Send('MDTM ' + FileName.QuotedString, R);
  if R.IsPass(200, 299) then
  begin
    S := R.Message;
    if S.Length <> 'YYYYMMDDhhmmss'.Length then
      Exit;
    Year := StrToIntDef(S.Substring(1, 4), 1970);
    Month := StrToIntDef(S.Substring(5, 2), 1);
    Day := StrToIntDef(S.Substring(7, 2), 1);
    Hour := StrToIntDef(S.Substring(9, 2), 0);
    Minute := StrToIntDef(S.Substring(11, 2), 0);
    Second := StrToIntDef(S.Substring(13, 2), 0);
    Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, 0);
  end;
end;

function TFtpClient.Passive(out Socket: TInetSocket): Boolean;
var
  R: TResponse;
  s:string;
  nb, ne: integer;
  sl:TStringList;

begin
  Socket := nil;
  Result := False;
  Send('PASV', R);
  if R.IsPass(200, 299) then
  begin

    s:=R.Message.Trim;
    nb := Pos('(',s);
    ne := Pos(')',s);
    if (nb = 0) or (ne = 0) then
    begin
      nb:=RPos(' ',s);
      s:=Copy(s, nb + 1, Length(s) - nb);
    end
    else
    begin
      s:=Copy(s,nb+1,ne-nb-1);
    end;

    sl:=TStringList.Create;
    try
      sl.CommaText:=s;
      if sl.Count<>6 then exit;
      Socket := TInetSocket.Create(sl[0]+'.'+sl[1]+'.'+sl[2]+'.'+sl[3],
                                 StrToIntDef(sl[4], 0)*256 + StrToIntDef(sl[5], 0));
    finally
      sl.Free;
    end;
  end
end;

function TFtpClient.FileModeBinary: Boolean;
var
  R: TResponse;
begin
  Send('TYPE I', R);
  Result := R.IsPass(200, 299);
end;

{
procedure TFtpClient.DoProgress(const Size, Transmitted: QWord);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Size, Transmitted);
end;
}

function TFtpClient.FilePut(const LocalFile, RemoteFile: string; Overwrite: Boolean = True): Boolean;
const
  BufferSize = 1024 * 1024;
var
  Socket: TInetSocket;
  Stream: TStream;
  Buffer: Pointer;
  SourceSize, DestSize: QWord;
  Count: LongWord;
  R: TResponse;
begin
  Result := False;
  if not FileExists(LocalFile) then
    Exit;
  if (not Overwrite) and FileExists(RemoteFile) then
    Exit;
  if not FileModeBinary then
    Exit;
  SourceSize := FileSize(LocalFile);
  DestSize := 0;
  if Passive(Socket) then
  try
    Send('STOR ' + RemoteFile.QuotedString, R);
    if R.IsFail(100, 299) then
      Exit;
    Stream := TFileStream.Create(LocalFile, fmOpenRead);
    GetMem(Buffer, BufferSize);
    FTransfering := True;
    try
      repeat
        Count := Stream.Read(Buffer^, BufferSize);
        if Count > 0 then
          if Socket.Write(Buffer^, Count)>0 then
          begin
            DestSize := DestSize + Count;
            //DoProgress(SourceSize, DestSize);
          end;
      until (not FTransfering) or (Count < BufferSize);
      Result := DestSize = SourceSize;
    finally
      FTransfering := False;
      FreeMem(Buffer);
      Stream.Free;
    end;
  finally
    Socket.Free;
  end;
end;

function TFtpClient.FileGet(const RemoteFile, LocalFile: string; Overwrite: Boolean = True): Boolean;
const
  BufferSize = 1024 * 1024;
var
  Socket: TInetSocket;
  Stream: TStream;
  Buffer: Pointer;
  SourceSize, DestSize: QWord;
  Count: LongInt;
  R: TResponse;
begin
  Result := False;
  if (not Overwrite) and FileExists(LocalFile) then
    Exit;
  if not FileModeBinary then
    Exit;
  SourceSize := FileSize(RemoteFile);
  DestSize := 0;
  if Passive(Socket) then
  try
    Send('RETR ' + RemoteFile.QuotedString, R);
    if R.IsFail(100, 299) then
      Exit;
    Stream := TFileStream.Create(LocalFile, fmCreate);
    GetMem(Buffer, BufferSize);
    FTransfering := True;
    try
      repeat
        Count := Socket.Read(Buffer^, BufferSize);
        if Count > 0 then
          if Stream.Write(Buffer^, Count) = Count then
          begin
            DestSize := DestSize + Count;
            //DoProgress(SourceSize, DestSize);
          end;
      until (not FTransfering) or (Count < 1);
      Result := DestSize = SourceSize;
    finally
      FTransfering := False;
      FreeMem(Buffer);
      Stream.Free;
    end;
  finally
    Socket.Free;
  end;
end;

function TFtpClient.FileList(const Path: string = ''): string;
var
  Socket: TInetSocket;
  R: TResponse;
  S: string;
begin
  Result := '';
  if Passive(Socket) then
  try
    if IsEmptyStr(Path,[#0,' ']) then
    //if Path.IsNullOrWhiteSpace(Path) then
      Send('LIST', R)
    else
      Send('LIST ' + Path.QuotedString, R);
    if R.IsPass(150, 299) then
    begin
      repeat
        S:=Socket.ReadAnsiString;
        Result := Result + S;
      until Length(S)=0;
    end;
    if R.IsPass(150, 199) then
      Recv(R);
  finally
    Socket.Free;
  end;
end;

function TFtpClient.FindFirst(const Path: string; out FindData: TRemoteFindData): Boolean;
var
  S: string;
begin
  S := FileList(Path).Trim;
  if S.IsEmpty then
  begin
    FindData.Name := '';
    FindData.Date := 0;
    FindData.Size := 0;
    Result := False;
  end
  else
  begin
    S := AdjustLineBreaks(S,tlbsCRLF);
    FFindList := S.Split(#13#10);
    FFindIndex := -1;
    Result := FindNext(FindData);
  end;
end;

function TFtpClient.FindNext(out FindData: TRemoteFindData): Boolean;
function Words(aSource:String; MaxColumns: Integer = 0): TStringArray;
var
  W: string;
  C, I: Integer;
  Cnt: Integer;
begin
  if MaxColumns < 1 then
    MaxColumns := High(Integer);
  W := '';
  C := 0;
  Cnt := 0;
  for I := 1 to Length(aSource) do
  begin
    if C >= MaxColumns then
      W := W + aSource[I]
    else if aSource[I] <= ' ' then
    begin
      if W.Length > 0 then
      begin
        SetLength(Result,Cnt+1);
        Result[Cnt]:=W;
        Inc(Cnt);
        Inc(C);
      end;
      W := '';
    end
    else
      W := W + aSource[I];
  end;
  if W.Length > 0 then
  begin
    SetLength(Result,Cnt+1);
    Result[Cnt]:=W;
  end;
end;
function SafeRead(var Columns: TStringArray; Index: Integer): string;
var
  I: Integer;
begin
  I := Length(Columns);
  if Index < I
     then Result := Columns[Index]
     else Result := '';
end;
const
  AttributeColumn = 0;
  SizeColumn = 4;
  MonthColumn = 5;
  DayColumn = 6;
  YearColumn = 7;
  FileColumn = 8;
var
  Columns: TStringArray;
  Coded: Boolean;
  S: string;
  Y, M, D: Word;
  T: Double;
  I: Integer;
begin
  Result := True;
  FindData.Name := '';
  FindData.Date := 0;
  FindData.Size := 0;
  Inc(FFindIndex);
  if FFindIndex < Length(FFindList) then
  begin
    Columns := Words(FFindList[FFindIndex],FileColumn);
    S := SafeRead(Columns, AttributeColumn);
    if S.Length >= 10 then
    begin
      {
      if S[1] = 'd' then
        Include(FindData.Attributes, fsaDirectory);
      if S[1] = 'l' then
        Include(FindData.Attributes, fsaLink);
      if S[8] = 'r' then
        Include(FindData.Attributes, fsaRead);
      if S[9] = 'w' then
        Include(FindData.Attributes, fsaWrite);
      if S[10] = 'x' then
        Include(FindData.Attributes, fsaExecute);
      }
    end;
    {
    if FindData.Attributes * FFindMask = [] then
    begin
      Result := FindNext(FindData);
      Exit;
    end;
    }
    FindData.Name := SafeRead(Columns, FileColumn);
    FindData.Size := StrToQWordDef(SafeRead(Columns, SizeColumn), 0);
    M := 1;
    for I := Low(FormatSettings.ShortMonthNames) to High(FormatSettings.ShortMonthNames) do
      if SafeRead(Columns, MonthColumn).Equals(FormatSettings.ShortMonthNames[I]) then
      begin
        M := I;
        Break;
      end;
    D := StrToIntDef(SafeRead(Columns, DayColumn), 1);
    S := SafeRead(Columns, YearColumn);
    Coded := S.Contains(':');
    if Coded then
    begin
      Y := YearOf(Now);
      T := StrToTime(S + ':00');
    end
    else
    begin
      Y := StrToIntDef(S, YearOf(Now));
      T := 0;
    end;
    FindData.Date := EncodeDate(Y, M, D) + T;
    if Coded and (FindData.Date > Now + 1) then
      FindData.Date := EncodeDate(Y - 1, M, D) + T;
    Result := True;
  end
  else
    Result := False;
end;

procedure TFtpClient.SetConnected(Value: Boolean);
begin
  if Value <> FConnected then
  begin
    if Value then
      Connect
    else
      Disconnect;
  end;
end;

function TFtpClient.GetConnected: Boolean;
begin
  Result := FConnected;
end;

end.

