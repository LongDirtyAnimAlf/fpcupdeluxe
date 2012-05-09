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

const
  // Custom return codes
  FRET_LOCAL_REMOTE_URL_NOMATCH= -2;
  FRET_WORKING_COPY_TOO_OLD= -3;

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
    // Makes sure non-empty strings have a / at the end.
    function IncludeTrailingSlash(AValue: string): string;
    procedure SetDesiredRevision(AValue: string);
    procedure SetRepositoryURL(AValue: string);
    procedure SetSVNExecutable(AValue: string);
    procedure SetVerbose(AValue: boolean);
  public
    //Performs an SVN checkout (initial download), unless otherwise specified HEAD (latest revision) only for speed
    procedure CheckOut;
    //Runs SVN checkout if local repository doesn't exist, else does an update
    procedure CheckOutOrUpdate;
    //Creates diff of all changes in the local directory versus the SVN version
    function GetDiffAll:string;
    //Search for installed SVN executable (might return just a filename if in the OS path)
    function FindSVNExecutable: string;
    //Shows commit log for local directory
    procedure Log(var Log: TStringList);
    //Reverts/removes local changes so we get a clean copy again. Note: will remove modifications to files!
    procedure Revert;
    //Performs an SVN update (pull)
    procedure Update;
    //Get/set desired revision to checkout/pull to (if none given, use HEAD)
    property DesiredRevision: string read FDesiredRevision write SetDesiredRevision;
    //Shows list of files that have been modified locally (and not committed)
    procedure LocalModifications(var FileList: TStringList);
    //Checks to see if local directory is a valid SVN repository for the repository URL given (if any)
    function LocalRepositoryExists: boolean;
    //Local directory that has an SVN repository/checkout
    property LocalRepository: string read FLocalRepository write FLocalRepository;
    //Revision number of local repository
    function LocalRevision: integer;
    //URL where central SVN repository is placed
    property Repository: string read FRepositoryURL write SetRepositoryURL;
    //Exit code returned by last SVN client command; 0 for success. Useful for troubleshooting
    property ReturnCode: integer read FReturnCode;
    //SVN client executable. Can be set to explicitly determine which executable to use.
    property SVNExecutable: string read GetSVNExecutable write SetSVNExecutable;
    //Show additional console/log output?
    property Verbose:boolean read FVerbose write SetVerbose;
    constructor Create;
    destructor Destroy; override;
  end;


implementation
uses strutils;


{ TSVNClient }
function TSVNClient.FindSvnExecutable: string;
const
  // Application name:
  SVNName = 'svn';
begin
  Result := FSVNExecutable;
  // Look in path
  // Windows: will also look for <SVNName>.exe
  if not FileExists(FSvnExecutable) then
    FSvnExecutable := FindDefaultExecutablePath(SVNName);

{$IFDEF MSWINDOWS}
  // Some popular locations for SlikSVN, Subversion, and TortoiseSVN:
  // Covers both 32 bit and 64 bit Windows.
  if not FileExists(FSvnExecutable) then
    FSvnExecutable := GetEnvironmentVariable('ProgramFiles\Subversion\bin\svn.exe');
  if not FileExists(FSvnExecutable) then
    FSvnExecutable := GetEnvironmentVariable('ProgramFiles(x86)\Subversion\bin\svn.exe');
  if not FileExists(FSvnExecutable) then
    FSvnExecutable := GetEnvironmentVariable('ProgramFiles\SlikSvn\bin\svn.exe');
  if not FileExists(FSvnExecutable) then
    FSvnExecutable := GetEnvironmentVariable('ProgramFiles(x86)\SlikSvn\bin\svn.exe');
  if not FileExists(FSvnExecutable) then
    FSvnExecutable := GetEnvironmentVariable('ProgramFiles\TorToiseSVN\bin\svn.exe');
  if not FileExists(FSvnExecutable) then
    FSvnExecutable := GetEnvironmentVariable('ProgramFiles(x86)\TorToiseSVN\bin\svn.exe');
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
    if ExecuteCommand(FSVNExecutable+ ' --version',Verbose) <> 0 then
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
    Result:=''
  else
    Result := FSVNExecutable;
end;

function TSVNClient.IncludeTrailingSlash(AValue: string): string;
begin
  // Default: either empty string or / already there
  result:=AValue;
  if (AValue<>'') and (RightStr(AValue,1)<>'/') then
  begin
    result:=AValue+'/';
  end;
end;


procedure TSVNClient.SetDesiredRevision(AValue: string);
begin
  if FDesiredRevision=AValue then Exit;
  FDesiredRevision:=AValue;
end;

procedure TSVNClient.SetRepositoryURL(AValue: string);
// Make sure there's a trailing / in the URL.
// This normalization helps matching remote and local URLs
begin
  if FRepositoryURL=AValue then Exit;
  FRepositoryURL:=IncludeTrailingSlash(AValue);
end;

procedure Tsvnclient.Checkout;
const
  MaxRetries = 3;
var
  Command: string;
  RetryAttempt: integer;
begin
  if (FDesiredRevision='') or (trim(FDesiredRevision)='HEAD') then
    Command := ' checkout --non-interactive -r HEAD ' + Repository + ' ' + LocalRepository
  else
    Command := ' checkout --non-interactive -r '+ FDesiredRevision+ ' ' + Repository + ' ' + LocalRepository;
  FReturnCode:=ExecuteCommand(SVNExecutable+Command,Verbose);
  // If command fails, e.g. due to misconfigured firewalls blocking ICMP etc, retry a few times
  RetryAttempt := 1;
  while (ReturnCode <> 0) and (RetryAttempt < MaxRetries) do
  begin
    Sleep(500); //Give everybody a chance to relax ;)
    FReturnCode:=ExecuteCommand(SVNExecutable+Command,Verbose); //attempt again
    RetryAttempt := RetryAttempt + 1;
  end;
end;

procedure Tsvnclient.CheckOutOrUpdate;
begin
  if LocalRepositoryExists = False then
  begin
    if FReturnCode=FRET_LOCAL_REMOTE_URL_NOMATCH then
    begin
      // We could delete the entire directory and checkout
      // but the user could take issue with that.
      // We already set the return code, so just let caller handle it.
    end
    else
    begin
      // Checkout (first download)
      Checkout;
    end;
  end
  else
  begin
    // Update
    Update;
  end;
end;

function TSVNClient.GetDiffAll:string;
begin
  FReturnCode:=ExecuteCommand(SVNExecutable+' diff ' + LocalRepository,Result,Verbose);
end;

procedure Tsvnclient.Log(var Log: TStringList);
var
  s:string='';
begin
  FReturnCode:=ExecuteCommand(SVNExecutable+' log ' + LocalRepository,s,Verbose);
  Log.Text:=s;
end;

procedure Tsvnclient.Revert;
begin
  FReturnCode:=ExecuteCommand(SVNExecutable+' revert --recursive ' + LocalRepository,Verbose);
end;

procedure TSVNClient.SetSVNExecutable(AValue: string);
begin
  if FSVNExecutable <> AValue then
  begin
    FSVNExecutable := AValue;
    FindSVNExecutable; //Make sure it actually exists; use fallbacks if possible
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
    Command := ' update --non-interactive ' + LocalRepository
  else
    Command := ' update --non-interactive -r ' + FDesiredRevision + ' ' + LocalRepository;
  FReturnCode:=ExecuteCommand(SVNExecutable+command,Verbose);

  // If command fails, e.g. due to misconfigured firewalls blocking ICMP etc, retry a few times
  RetryAttempt := 1;
  while (ReturnCode <> 0) and (RetryAttempt < MaxRetries) do
  begin
    Sleep(500); //Give everybody a chance to relax ;)
    FReturnCode:=ExecuteCommand(SVNExecutable+command,Verbose); //attempt again
    RetryAttempt := RetryAttempt + 1;
  end;
end;

procedure TSVNClient.LocalModifications(var FileList: TStringList);
var
  AllFiles: TStringList;
  Counter: integer;
  Output: string='';
  StatusCode: string;
begin
  FReturnCode:=ExecuteCommand(SVNExecutable+' status --depth infinity '+FLocalRepository,Output,Verbose);
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
const
  URLLen=Length('URL: ');
var
  Output:string='';
  URL: string;
  URLPos: integer;
begin
  Result := False;
  FReturnCode:=ExecuteCommand(SVNExecutable+' info ' + FLocalRepository,Output,Verbose);
  //This is already covered by setting stuff to false first
  //if Pos('is not a working copy', Output.Text) > 0 then result:=false;
  if Pos('Path', Output) > 0 then
  begin
    // There is an SVN repository here.
    // Output from info command can include:
    // URL: http://svn.freepascal.org/svn/fpc/branches/fixes_2_6
    // Repository URL might differ from the one we've set though
    URLPos:=pos('URL: ', Output)+URLLen;
    URL:= IncludeTrailingSlash(trim(copy(Output,
      (URLPos), Posex(LineEnding,Output,URLPos)-URLPos )));
    if FRepositoryURL='' then
    begin
      FRepositoryURL:=URL;
      Result:=true;
    end
    else
    begin
      if FRepositoryURL=URL then
      begin
        result:=true;
      end
      else
      begin
        // There is a repository here, but it was checked out
        // from a different URL...
        // Keep result false; show caller what's going on.
        FReturnCode:=FRET_LOCAL_REMOTE_URL_NOMATCH;
      end;
    end;
  end;
end;

function TSVNClient.LocalRevision: integer;
const
  RevLength = Length('Revision:');
var
  LRevision: string;
begin
  result:=-1;
  FReturnCode:=ExecuteCommand(SVNExecutable+' info ' + FLocalRepository,LRevision,Verbose);
  // Could have used svnversion but that would have meant calling yet another command...
  // Get the part after "DesiredRevision:"
  if FReturnCode=0 then
    Result := StrToIntDef(trim(copy(LRevision,
      (pos('Revision: ', LRevision) + RevLength),
      6)), -1)
  else
    if Pos('E155036',LRevision)>0 then
      begin
      result:=FRET_WORKING_COPY_TOO_OLD;
      end;
end;


constructor Tsvnclient.Create;
begin
  FLocalRepository := '';
  FRepositoryURL := '';
  FDesiredRevision:='';
  FReturnCode := 0;
  FSVNExecutable := '';
  FindSvnExecutable; //Do this now so hopefully the SVNExecutable property is valid.
end;

destructor Tsvnclient.Destroy;
begin
  inherited Destroy;
end;
end.
