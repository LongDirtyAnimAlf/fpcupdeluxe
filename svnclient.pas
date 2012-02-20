{ Classes for using svn commands
  Copyright (C) 2012 Reinier Olislagers, Ludo Brands

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
  processutils,
  FileUtil {Requires LCL};

type
  ESVNClientError = class(Exception);
  { TSVNClient }

  TSVNClient = class(TObject)
  private
    FLocalRepository: string;
    FRepositoryURL: string;
    FReturnCode: integer;
    FDesiredRevision: string;
    FSVNExecutable: string;
    FVerbose: boolean;
    function GetSVNExecutable: string;
    procedure SetDesiredRevision(AValue: string);
    procedure SetSVNExecutable(AValue: string);
    procedure SetVerbose(AValue: boolean);
  public
    procedure CheckOut;
    //Performs an SVN checkout (initial download), unless otherwise specified HEAD (latest revision) only for speed
    procedure CheckOutOrUpdate;
    //Runs SVN checkout if local repository doesn't exist, else does an update
    function GetDiffAll:string; //Creates diff of all changes
    function FindSVNExecutable: string;
    //Search for installed SVN executable (might return just a filename if in the OS path)
    procedure Log(var Log: TStringList); //Shows commit log for local directory
    procedure Revert;
    //Reverts/removes local changes so we get a clean copy again. Note: will remove modifications to files!
    procedure Update; //Performs an SVN update (pull)
    property DesiredRevision: string read FDesiredRevision write SetDesiredRevision;
    //Get/set desired revision to pull to (if none given, use HEAD)
    procedure LocalModifications(var FileList: TStringList);
    //Shows list of files that have been modified locally (and not committed)
    function LocalRepositoryExists: boolean;
    //Checks to see if local directory is a valid SVN repository
    property LocalRepository: string read FLocalRepository write FLocalRepository;
    //Local directory that has an SVN repository/checkout
    function LocalRevision: integer; //Revision number of local repository
    property Repository: string read FRepositoryURL write FRepositoryURL;
    //URL where central SVN repository is placed
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
begin
  Result := FSVNExecutable;
  // Look in path
  // Windows: will also look for <SVNName>.exe
  if not FileExists(FSvnExecutable) then
    FSvnExecutable := FindDefaultExecutablePath(SVNName);

{$IFDEF MSWINDOWS}
  // Some popular locations for SlikSVN and Subversion:
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
  //Directory where current executable is:
  if not FileExists(FSvnExecutable) then
    FSvnExecutable := (ExtractFilePath(ParamStr(0)) + 'svn');
{$ENDIF MSWINDOWS}

  if not FileExists(FSvnExecutable) then
  begin
    //current directory. Note: potential for misuse by malicious program.
    if FileExists(SVNName+'.exe') then
      FSVNExecutable := SVNName+'.exe';
    if FileExists('svn') then
      FSVNExecutable := SVNName;
  end;

  if FileExists(FSVNExecutable) then
  begin
    // Check for valid svn executable
    if ExecuteCommandHidden(FSVNExecutable, '--version',Verbose) <> 0 then
    begin
      // File exists, but is not a valid svn client
      FSVNExecutable := EmptyStr;
    end;
  end
  else
  begin
    // File does not exist
    // Make sure we don't call an arbitrary executable:
    FSVNExecutable := EmptyStr;
  end;
  Result := FSVNExecutable;
end;

function TSVNClient.GetSVNExecutable: string;
begin
  if not FileExists(FSVNExecutable) then FindSVNExecutable;
  if not FileExists(FSVNExecutable) then
    raise ESVNClientError.create('No SVN executable found');
  Result := FSVNExecutable;
end;

procedure TSVNClient.SetDesiredRevision(AValue: string);
begin
  if FDesiredRevision=AValue then Exit;
  FDesiredRevision:=AValue;
end;

procedure Tsvnclient.Checkout;
const
  MaxRetries = 3;
var
  Command: string;
  RetryAttempt: integer;
begin
  if (FDesiredRevision='') or (trim(FDesiredRevision)='HEAD') then
    Command := 'checkout --non-interactive -r HEAD ' + Repository + ' ' + LocalRepository
  else
    Command := 'checkout --non-interactive -r '+ FDesiredRevision+ ' ' + Repository + ' ' + LocalRepository;
  FReturnCode:=ExecuteCommandHidden(SVNExecutable,Command,Verbose);
  // If command fails, e.g. due to misconfigured firewalls blocking ICMP etc, retry a few times
  RetryAttempt := 1;
  while (ReturnCode <> 0) and (RetryAttempt < MaxRetries) do
  begin
    Sleep(500); //Give everybody a chance to relax ;)
    FReturnCode:=ExecuteCommandHidden(SVNExecutable,Command,Verbose); //attempt again
    RetryAttempt := RetryAttempt + 1;
  end;
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

function TSVNClient.GetDiffAll:string;
begin
  FReturnCode:=ExecuteCommandHidden(SVNExecutable,'diff ' + LocalRepository,Result,Verbose);
end;

procedure Tsvnclient.Log(var Log: TStringList);
var
  s:string;
begin
  FReturnCode:=ExecuteCommandHidden(SVNExecutable,'log ' + LocalRepository,s,Verbose);
  Log.Text:=s;
end;

procedure Tsvnclient.Revert;
begin
  FReturnCode:=ExecuteCommandHidden(SVNExecutable,'revert --recursive ' + LocalRepository,Verbose);
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
begin
  if (FDesiredRevision='') or (trim(FDesiredRevision)='HEAD') then
    Command := 'update --non-interactive ' + LocalRepository
  else
    Command := 'update --non-interactive -r ' + FDesiredRevision + ' ' + LocalRepository;
  FReturnCode:=ExecuteCommandHidden(SVNExecutable,command,Verbose);

  // If command fails, e.g. due to misconfigured firewalls blocking ICMP etc, retry a few times
  RetryAttempt := 1;
  while (ReturnCode <> 0) and (RetryAttempt < MaxRetries) do
  begin
    Sleep(500); //Give everybody a chance to relax ;)
    FReturnCode:=ExecuteCommandHidden(SVNExecutable,command,Verbose); //attempt again
    RetryAttempt := RetryAttempt + 1;
  end;
end;

procedure TSVNClient.LocalModifications(var FileList: TStringList);
var
  AllFiles: TStringList;
  Counter: integer;
  Output: string;
  StatusCode: string;
begin
  FReturnCode:=ExecuteCommandHidden(SVNExecutable,'status --depth infinity '+FLocalRepository,Output,Verbose);
  AllFiles:=TStringList.Create;
  try
    AllFiles.Text:=Output;
    for Counter := 0 to AllFiles.Count - 1 do
    begin
      //sample:
      //M       C:\Development\fpc\packages\bzip2\Makefile
      //123456789
      StatusCode:=Copy(AllFiles[Counter],1,1);
      // there is probably a much more set-oriented Pascal way to do this ;)
      //(M)odified, (C)onflicting, mer(G)ed automatically)
      if (StatusCode='C') or (StatusCode='G') or (StatusCode='M') then
      begin
        FileList.Add(Copy(AllFiles[Counter],9,Length(AllFiles[Counter])));
      end;
    end;
  finally
    AllFiles.Free;
  end;
end;

function Tsvnclient.LocalRepositoryExists: boolean;
var
  s:string;
begin
    Result := False;
    FReturnCode:=ExecuteCommandHidden(SVNExecutable,'info ' + FLocalRepository,s,Verbose);
    if Pos('Path', s) > 0 then
      Result := True;
    //This is already covered by setting stuff to false first
    //if Pos('is not a working copy', Output.Text) > 0 then result:=false;
end;

function TSVNClient.LocalRevision: integer;
const
  RevLength = Length('Revision:');
var
  LRevision: string;
begin
  result:=-1;
  FReturnCode:=ExecuteCommandHidden(SVNExecutable,'info ' + FLocalRepository,LRevision,Verbose);
  // Could have used svnversion but that would have meant calling yet another command...
  // Get the part after "DesiredRevision:"
  Result := StrToIntDef(trim(copy(LRevision,
    (pos('Revision: ', LRevision) + RevLength),
    6)), -1);
end;


constructor Tsvnclient.Create;
begin
  FLocalRepository := '';
  FRepositoryURL := '';
  FDesiredRevision:='';
  FReturnCode := 0;
  FSVNExecutable := '';
  FindSvnExecutable; //Do this now so the SVNExecutable property is valid.
end;

destructor Tsvnclient.Destroy;
begin
  inherited Destroy;
end;
end.
