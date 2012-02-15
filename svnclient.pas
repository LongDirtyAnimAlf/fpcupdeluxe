{ Classes for using svn commands
  Copyright (C) 2012 Reinier Olislagers

  Based on svncommand unit
  Copyright (C) 2007 Vincent Snijders vincents@freepascal.org,

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
unit svnclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  process,
  FileUtil {Requires LCL};

type
  ESVNClientError = class(Exception);
  { TSVNClient }

  TSVNClient = class(TObject)
  private
    FLocalRepository: string;
    FRepositoryURL: string;
    FReturnCode: integer;
    FRevision: string;
    FSVNExecutable: string;
    FVerbose: boolean;
    function ExecuteCommand(const Executable, Parameters: string; Output: TStream): integer;
    // Execute external command; put stdout in Output; return exitcode
    function GetSVNExecutable: string;
    procedure SetRevision(AValue: string);
    procedure SetSVNExecutable(AValue: string);
    procedure SetVerbose(AValue: boolean);
  public
    procedure CheckOut;
    //Performs an SVN checkout (initial download), HEAD (latest revision) only for speed
    procedure CheckOutOrUpdate;
    //Runs SVN checkout if local repository doesn't exist, else does an update
    function FindSVNExecutable: string;
    //Search for installed SVN executable (might return just a filename if in the OS path)
    procedure Log(var Log: TStringList); //Shows commit log for local directory
    procedure Revert;
    //Reverts/removes local changes so we get a clean copy again. Note: will remove modifications to files!
    procedure Update; //Performs an SVN update (pull)
    function ExecuteSvnCommand(const Command: string; Output: TStream): integer;
    //Executes a free form SVN command; puts output into stream; returns SVN client exit code
    function ExecuteSVNCommand(const Command: string; var Output: TStringList): integer;
    //Executes a free form SVN command; puts output into stringlist; returns SVN client exit code
    function ExecuteSvnCommand(const Command: string): integer;
    //Executes a free form SVN command; returns SVN client exit code
    function LocalRepositoryExists: boolean;
    //Checks to see if local directory is a valid SVN repository
    property LocalRepository: string read FLocalRepository write FLocalRepository;
    //Local directory that has an SVN repository/checkout
    function LocalRevision: integer; //Revision number of local repository
    property Repository: string read FRepositoryURL write FRepositoryURL;
    //URL where central SVN repository is placed
    property Revision: string read FRevision write SetRevision;
    //Get/set desired revision to pull to (if none given, use HEAD)
    property ReturnCode: integer read FReturnCode;
    //Exit code returned by last SVN client command. Useful for troubleshooting
    property SVNExecutable: string read GetSVNExecutable write SetSVNExecutable;
    //SVN client executable. Can be set to explicitly determine which executable to use.
    property Verbose:boolean read FVerbose write SetVerbose;
    constructor Create;
    destructor Destroy; override;
  end;


implementation


{ TSVNClient }
function TSVNClient.FindSvnExecutable: string;
const
  SVNName = 'svn';
var
  ExeResult: longint;
  CommandOutput: TMemoryStream;
begin
  Result := FSVNExecutable;
  if FileExists(FSvnExecutable) then
  begin
    // File exists, assume it is a working svn client.
    exit;
  end;

  if FSVNExecutable = '' then
  begin
    //todo: check what happens if svn exe is in path but not specified here?
    // process call will still work!!?!
    CommandOutput:=TMemoryStream.Create;
    try
      ExeResult := ExecuteCommand(SVNName, '--version', CommandOutput);
      if ExeResult = 0 then
      begin
        //Found a working SVN in path
        FSVNExecutable := SVNName;
        exit;
      end;
    finally
      CommandOutput.Free;
    end;
  end;

{$IFDEF MSWINDOWS}
  // Some popular locations for SlikSVN and Subversion
  if not FileExists(FSvnExecutable) then
    FSvnExecutable := GetEnvironmentVariable('ProgramFiles') + '\Subversion\bin\svn.exe';
  if not FileExists(FSvnExecutable) then
    FSvnExecutable := GetEnvironmentVariable('ProgramFiles(x86)') +
      '\Subversion\bin\svn.exe';
  if not FileExists(FSvnExecutable) then
    FSvnExecutable := GetEnvironmentVariable('ProgramFiles') + '\SlikSvn\bin\svn.exe';
  if not FileExists(FSvnExecutable) then
    FSvnExecutable := GetEnvironmentVariable('ProgramFiles(x86)') +
      '\SlikSvn\bin\svn.exe';
{$ENDIF MSWINDOWS}

  if not FileExists(FSvnExecutable) then
    FSvnExecutable := FindDefaultExecutablePath('svn');

{$IFDEF MSWINDOWS}
  if not FileExists(FSvnExecutable) then
    FSvnExecutable := (ExtractFilePath(ParamStr(0)) + 'svn'); //directory where current executable is
{$ENDIF MSWINDOWS}

  if not FileExists(FSvnExecutable) then
  begin
    //current path.
    //todo: check if this is safe (e.g. compromised svn etc)
    if FileExists('svn.exe') then
      FSVNExecutable := 'svn.exe';
    if FileExists('svn') then
      FSVNExecutable := 'svn';
  end;

  if not FileExists(FSVNExecutable) then
    FSVNExecutable := ''; //Make sure we don't call an arbitrary executable
  Result := FSVNExecutable;
end;

function TSVNClient.ExecuteCommand(const Executable, Parameters: string; Output: TStream
  ): integer;
var
  ExternalProcess: TProcess;

  function ReadOutput: boolean;
    // returns true if output was actually read
  const
    BufSize = 4096;
  var
    Buffer: array[0..BufSize - 1] of byte;
    ReadBytes: integer;
  begin
    Result := False;
    while ExternalProcess.Output.NumBytesAvailable > 0 do
    begin
      ReadBytes := ExternalProcess.Output.Read(Buffer, BufSize);
      Output.Write(Buffer, ReadBytes);
      if Verbose then
        write(copy(pchar(@buffer[0]),1,ReadBytes));
      Result := True;
    end;
  end;

begin
  FReturnCode := 255; //Preset to failure
  // We could have checked for executable; but at least Windows
  // will also look in the path, so don't do that.

  ExternalProcess := TProcess.Create(nil);
  try
    ExternalProcess.CommandLine := Executable + ' ' + Parameters;
    ExternalProcess.Options := [poUsePipes, poStderrToOutPut];
    ExternalProcess.ShowWindow := swoHIDE;
    ExternalProcess.Execute;
    while ExternalProcess.Running do
    begin
      if not ReadOutput then
        Sleep(100);
    end;
    ReadOutput;
    FReturnCode := ExternalProcess.ExitStatus;
    Result := FReturnCode;
  finally
    ExternalProcess.Free;
  end;
end;

function TSVNClient.GetSVNExecutable: string;
begin
  Result := FSVNExecutable;
end;

procedure TSVNClient.SetRevision(AValue: string);
begin
  if FRevision=AValue then Exit;
  FRevision:=AValue;
end;

procedure Tsvnclient.Checkout;
const
  MaxRetries = 3;
var
  Command: string;
  RetryAttempt: integer;
begin
  if FRevision='' then
    Command := 'checkout --non-interactive --revision HEAD ' + Repository + ' ' + LocalRepository
  else
    Command := 'checkout --non-interactive -r '+ FRevision+ ' ' + Repository + ' ' + LocalRepository;
  ExecuteSVNCommand(Command);
  // If command fails, e.g. due to misconfigured firewalls blocking ICMP etc, retry a few times
  RetryAttempt := 1;
  while (ReturnCode <> 0) and (RetryAttempt < MaxRetries) do
  begin
    Sleep(500); //Give everybody a chance to relax ;)
    ExecuteSVNCommand(Command); //attempt again
    RetryAttempt := RetryAttempt + 1;
  end;
  FRevision:=''; //don't reuse
end;

procedure Tsvnclient.CheckOutOrUpdate;

begin
  if LocalRepositoryExists = False then
  begin
    // Checkout (first download)
    Checkout;
  end
  else
  begin
    // Update
    Update;
  end;
end;

procedure Tsvnclient.Log(var Log: TStringList);
begin
  ExecuteSVNCommand('log ' + LocalRepository, Log);
end;

procedure Tsvnclient.Revert;
begin
  ExecuteSVNCommand('revert --recursive ' + LocalRepository);
end;

procedure TSVNClient.SetSVNExecutable(AValue: string);
begin
  if FSVNExecutable <> AValue then
  begin
    FSVNExecutable := AValue;
    FindSVNExecutable; //Make sure it actually exists
  end;
end;

procedure TSVNClient.SetVerbose(AValue: boolean);
begin
  if FVerbose=AValue then Exit;
  FVerbose:=AValue;
end;

procedure Tsvnclient.Update;
const
  MaxRetries = 3;
var
  Command: string;
  RetryAttempt: integer;
  StartRevision: integer;
begin
  StartRevision := LocalRevision;
  if FRevision='' then
    Command := 'update --non-interactive ' + LocalRepository
  else
    Command := 'update --non-interactive -r ' + FRevision + ' ' + LocalRepository;
  ExecuteSVNCommand(Command);
  // If command fails, e.g. due to misconfigured firewalls blocking ICMP etc, retry a few times
  RetryAttempt := 1;
  while (ReturnCode <> 0) and (RetryAttempt < MaxRetries) do
  begin
    Sleep(500); //Give everybody a chance to relax ;)
    ExecuteSVNCommand(Command); //attempt again
    RetryAttempt := RetryAttempt + 1;
  end;
  FRevision:=''; //don't reuse
end;

function TSVNClient.ExecuteSvnCommand(const Command: string; Output: TStream): integer;
begin
  FReturnCode := 255; //Preset to failure
  // Look for SVN if necessary; error if needed:
  if not FileExists(FSVNExecutable) then FindSvnExecutable;
  if not FileExists(FSvnExecutable) then
    raise ESVNClientError.Create('No SVN executable found');
  FReturnCode:=ExecuteCommand(FSVNExecutable, Command, Output);
  Result := FReturnCode;
end;

function TSVNClient.ExecuteSVNCommand(const Command: string;
  var Output: TStringList): integer;
var
  OutputStream: TMemoryStream;
begin
  OutputStream := TMemoryStream.Create;
  try
    Result := ExecuteSvnCommand(Command, OutputStream);
    OutputStream.Position := 0;
    Output.LoadFromStream(OutputStream); //load output
  finally
    OutputStream.Free;
  end;
end;

function TSVNClient.ExecuteSvnCommand(const Command: string): integer;
var
  OutputStream: TMemoryStream;
begin
  OutputStream := TMemoryStream.Create;
  try
    Result := ExecuteSvnCommand(Command, OutputStream);
  finally
    OutputStream.Free;
  end;
end;

function Tsvnclient.LocalRepositoryExists: boolean;
var
  Output: TStringList;
begin
  Output := TStringList.Create;
  try
    Result := False;
    ExecuteSVNCommand('info ' + FLocalRepository, Output);
    if Pos('Path', Output.Text) > 0 then
      Result := True;
    //This is already covered by setting stuff to false first
    //if Pos('is not a working copy', Output.Text) > 0 then result:=false;
  finally
    Output.Free;
  end;
end;

function TSVNClient.LocalRevision: integer;
const
  RevLength = Length('Revision:');
var
  Output: TStringList;
  LRevision: string;
begin
  result:=-1;
  Output := TStringList.Create;
  try
    ExecuteSVNCommand('info ' + FLocalRepository, Output);
    // Could have used svnversion but that would have meant calling yet another command...
    LRevision := Output.Text;
    // Get the part after "Revision:"
    Result := StrToIntDef(trim(copy(LRevision,
      (pos('Revision: ', LRevision) + RevLength),
      6)), -1);
  finally
    Output.Free;
  end;
end;


constructor Tsvnclient.Create;
begin
  FLocalRepository := '';
  FRepositoryURL := '';
  FRevision:='';
  FReturnCode := 0;
  FSVNExecutable := '';
  FindSvnExecutable; //Do this now so the SVNExecutable property is valid.
end;

destructor Tsvnclient.Destroy;
begin
  inherited Destroy;
end;
end.
