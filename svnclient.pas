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

  { TSVNClient }

  TSVNClient = class(TObject)
  private
    FLocalRepository: string;
    FRepositoryURL: string;
    FReturnCode: integer;
    FSVNExecutable: string;
    procedure FindSVNExecutable;
  public
    procedure CheckOut;
    //Performs an SVN checkout (initial download), HEAD (latest revision) only for speed
    procedure CheckOutOrUpdate;
    //Pulls SVN checkout if local repository doesn't exist, else does an update
    procedure Log(var Log: TStringList); //Shows commit log for local directory
    procedure Revert;
    //Reverts/removes local changes so we get a clean copy again. Note: will remove modifications to files!
    procedure Update; //Performs an SVN update (pull)
    function ExecuteSvnCommand(const Command: string; Output: TStream): integer;
    //Executes a free form SVN command; returns SVN client exit code
    function ExecuteSVNCommand(const Command: string; var Output: TStringList): integer;
    //Executes a free form SVN command; returns SVN client exit code
    function ExecuteSvnCommand(const Command: string): integer;
    //Executes a free form SVN command; returns SVN client exit code
    function LocalRepositoryExists: boolean;
    property LocalRepository: string read FLocalRepository write FLocalRepository;
    //Local directory that has an SVN repository/checkout
    property Repository: string read FRepositoryURL write FRepositoryURL;
    property ReturnCode: integer read FReturnCode;
    //Exit code returned by last SVN client command. Useful for troubleshooting
    property SVNExecutable: string read FSVNExecutable write FSVNExecutable;
    constructor Create;
    destructor Destroy; override;
  end;


implementation


{ TSVNClient }
procedure TSVNClient.FindSvnExecutable;
begin
  //todo: add current directory for windows
  if FileExists(FSvnExecutable) then
    exit;
{$IFDEF windows}
  FSvnExecutable := GetEnvironmentVariable('ProgramFiles') + '\Subversion\bin\svn.exe';
{$ENDIF}

  if not FileExists(FSvnExecutable) then
    FSvnExecutable := FindDefaultExecutablePath('svn');

{$IFDEF windows}
  if not FileExists(FSvnExecutable) then
    FSvnExecutable := (ExtractFilePath(ParamStr(0)) + 'svn'); //executable directory
  if not FileExists(FSvnExecutable) then
    FSvnExecutable := ('.\svn'); //current directory
{$ENDIF}
  if not FileExists(FSvnExecutable) then
    raise Exception.Create('No SVN executable found');
end;

procedure Tsvnclient.Checkout;
const
  MaxRetries = 3;
var
  Command: string;
  RetryAttempt: integer;
begin
  Command := 'checkout --revision HEAD ' + Repository + ' ' + LocalRepository;
  ExecuteSVNCommand(Command);
  // If command fails, e.g. due to misconfigured firewalls blocking ICMP etc, retry a few times
  RetryAttempt := 1;
  while (ReturnCode <> 0) and (RetryAttempt < MaxRetries) do
  begin
    Sleep(500); //Give everybody a chance to relax ;)
    ExecuteSVNCommand(Command); //attempt again
    RetryAttempt := RetryAttempt + 1;
  end;
end;

procedure Tsvnclient.CheckOutOrUpdate;

begin
  if LocalRepositoryExists = False then
  begin
    // Checkout (first download)
    //writeln('debug: doing checkout of ' + Repository + ' to ' + LocalRepository + '.');
    Checkout;
  end
  else
  begin
    // Update
    //writeln('debug: doing update of ' + Repository + ' to ' + LocalRepository + '.');
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

procedure Tsvnclient.Update;
const
  MaxRetries = 3;
var
  Command: string;
  RetryAttempt: integer;
begin
  Command := 'update ' + LocalRepository;
  ExecuteSVNCommand(Command);
  // If command fails, e.g. due to misconfigured firewalls blocking ICMP etc, retry a few times
  RetryAttempt := 1;
  while (ReturnCode <> 0) and (RetryAttempt < MaxRetries) do
  begin
    Sleep(500); //Give everybody a chance to relax ;)
    ExecuteSVNCommand(Command); //attempt again
    RetryAttempt := RetryAttempt + 1;
  end;
end;

function TSVNClient.ExecuteSvnCommand(const Command: string; Output: TStream): integer;
var
  SvnProcess: TProcess;

  function ReadOutput: boolean;
    // returns true if output was actually read
  const
    BufSize = 4096;
  var
    Buffer: array[0..BufSize - 1] of byte;
    ReadBytes: integer;
  begin
    Result := False;
    while SvnProcess.Output.NumBytesAvailable > 0 do
    begin
      ReadBytes := SvnProcess.Output.Read(Buffer, BufSize);
      Output.Write(Buffer, ReadBytes);
      Result := True;
    end;
  end;

  function GetOutput: string;
  begin
    SetLength(Result, Output.Size);
    Output.Seek(0, soBeginning);
    Output.Read(Result[1], Length(Result));
  end;

begin
  FReturnCode := 255; //Reset to failure
  if FSvnExecutable = '' then
    FindSvnExecutable;

  SvnProcess := TProcess.Create(nil);
  try
    SvnProcess.CommandLine := SvnExecutable + ' ' + Command;
    SvnProcess.Options := [poUsePipes, poStderrToOutPut];
    SvnProcess.ShowWindow := swoHIDE;
    SvnProcess.Execute;
    while SvnProcess.Running do
    begin
      if not ReadOutput then
        Sleep(100);
    end;
    ReadOutput;
    FReturnCode := SvnProcess.ExitStatus;
    Result := FReturnCode;
  finally
    SvnProcess.Free;
  end;
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
    {
    writeln('debug: svn command: ' + command + ' gives this output:');
    writeln('debug: *** begin');
    writeln(Output.Text);
    writeln('debug: *** end');
    }
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
    if (ExecuteSVNCommand('info ' + FLocalRepository, Output) = 0) then
      Result := False;
    if Pos('Path', Output.Text) > 0 then
      Result := True;
    //This is already covered by setting stuff to false first
    //if Pos('is not a working copy', Output.Text) > 0 then result:=false;
  finally
    Output.Free;
  end;
end;


constructor Tsvnclient.Create;
begin
  FindSVNExecutable;
  FLocalRepository := '';
  FRepositoryURL := '';
  FReturnCode := 0;
  FSVNExecutable := '';
end;

destructor Tsvnclient.Destroy;
begin
  inherited Destroy;
end;


end.

