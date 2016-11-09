{ Classes for using mercurial/hg commands
  Copyright (C) 2012-2013 Reinier Olislagers, Ludo Brands

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



unit hgclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  processutils,
  FileUtil {Requires LCL}, repoclient;

const
  // Custom return codes
  FRET_LOCAL_REMOTE_URL_NOMATCH = repoclient.FRET_LOCAL_REMOTE_URL_NOMATCH;
  FRET_WORKING_COPY_TOO_OLD = repoclient.FRET_WORKING_COPY_TOO_OLD;
  FRET_UNKNOWN_REVISION = repoclient.FRET_UNKNOWN_REVISION;

type
  EHGClientError = class(ERepoClientError);
  { ThgClient }

  THGClient = class(TRepoClient)
  protected
    procedure CheckOut(UseForce:boolean=false); override;
    function GetLocalRevision: string; override;
    // Returns command snippet to set HTTP proxy config variables if needed
    function GetProxyCommand: string;
    function GetRepoExecutable: string; override;
    function GetRepoExecutableName: string; override;
    procedure Update; override;
  public
    procedure CheckOutOrUpdate; override;
    function Commit(Message: string): boolean; override;
    function Execute(Command: string): integer; override;
    function GetDiffAll: string; override;
    function FindRepoExecutable: string; override;
    procedure LocalModifications(var FileList: TStringList); override;
    function LocalRepositoryExists: boolean; override;
    procedure Log(var Log: TStringList); override;
    procedure ParseFileList(const CommandOutput: string; var FileList: TStringList; const FilterCodes: array of string); override;
    procedure Revert; override;
    constructor Create;
    destructor Destroy; override;
  end;


implementation

uses
  fpcuputil,
  strutils;


{ ThgClient }
function THGClient.GetRepoExecutableName: string;
begin
  // Application name:
  result := 'hg';
end;

function THGClient.FindRepoExecutable: string;
begin
  Result := FRepoExecutable;
  // Look in path
  // Windows: will also look for <hgName>.exe
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := FindDefaultExecutablePath(RepoExecutableName);

{$IFDEF MSWINDOWS}
  // Some popular locations for Tortoisehg:
  // Covers both 32 bit and 64 bit Windows.
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := GetEnvironmentVariable('ProgramFiles\TorToisehg\' + RepoExecutableName + '.exe');
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := GetEnvironmentVariable('ProgramFiles(x86)\TorToisehg\' + RepoExecutableName + '.exe');
  //Directory where current executable is:
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := (SafeGetApplicationPath + RepoExecutableName + '.exe');
{$ENDIF MSWINDOWS}

  if not FileExists(FRepoExecutable) then
  begin
    //current directory. Note: potential for misuse by malicious program.
    {$IFDEF MSWINDOWS}
    if FileExists(RepoExecutableName + '.exe') then
      FRepoExecutable := RepoExecutableName + '.exe';
    {$ELSE}
    if FileExists(RepoExecutableName) then
      FRepoExecutable := RepoExecutableName;
    {$ENDIF MSWINDOWS}
  end;

  if FileExists(FRepoExecutable) then
  begin
    // Check for valid hg executable
    if ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + ' --version', Verbose) <> 0 then
    begin
      // File exists, but is not a valid hg client
      FRepoExecutable := '';
    end;
  end
  else
  begin
    // File does not exist
    // Make sure we don't call an arbitrary executable:
    FRepoExecutable := '';
  end;
  Result := FRepoExecutable;
end;

function THGClient.GetRepoExecutable: string;
begin
  if not FileExists(FRepoExecutable) then
    FindRepoExecutable;
  if not FileExists(FRepoExecutable) then
    Result := ''
  else
    Result := FRepoExecutable;
end;

procedure THGClient.CheckOut(UseForce:boolean=false);
const
  MaxRetries = 3;
var
  Command: string;
  Output: string = '';
  RetryAttempt: integer;
begin
  // Invalidate our revision number cache
  FLocalRevision := FRET_UNKNOWN_REVISION;

  //tip is similar to svn HEAD
  // Could add --insecure to ignore certificate problems, but rather not
  if (FDesiredRevision = '') or (trim(FDesiredRevision) = 'tip') then
    Command := ' '+GetProxyCommand+' clone -r tip ' + Repository + ' ' + LocalRepository
  else
    Command := ' '+GetProxyCommand+' clone -r ' + FDesiredRevision + ' ' + Repository + ' ' + LocalRepository;
  FReturnCode := ExecuteCommand(RepoExecutable + Command, Output, FVerbose);
  // If command fails, e.g. due to misconfigured firewalls blocking ICMP etc, retry a few times
  RetryAttempt := 1;
  if (ReturnCode <> 0) then
  begin
    while (ReturnCode <> 0) and (RetryAttempt < MaxRetries) do
    begin
      Sleep(500); //Give everybody a chance to relax ;)
      FReturnCode := ExecuteCommand(RepoExecutable + Command, Output, FVerbose); //attempt again
      RetryAttempt := RetryAttempt + 1;
    end;
  end;
end;

procedure THGClient.CheckOutOrUpdate;
begin
  if LocalRepositoryExists = false then
  begin
    if FReturnCode = FRET_LOCAL_REMOTE_URL_NOMATCH then
    begin
      // We could delete the entire directory and Clone
      // but the user could take issue with that.
      // We already set the return code, so just let caller handle it.
    end
    else
    begin
      // Clone (first download)
      Checkout;
      // Just to be sure, update as well (Clone may have failed without warning):
      Update;
    end;
  end
  else
  begin
    // Update
    Update;
  end;
end;

function THGClient.Commit(Message: string): boolean;
begin
  inherited Commit(Message);
  FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' '+GetProxyCommand+' commit --message '+Message, LocalRepository, Verbose);
  //todo: do pushafter to push to remote repo?
  Result:=(FReturnCode=0);
end;

function THGClient.Execute(Command: string): integer;
begin
  FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' '+Command+' '+GetProxyCommand+' ', LocalRepository, Verbose);
  Result := FReturnCode;
end;

function THGClient.GetDiffAll: string;
begin
  FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable)+' '+GetProxyCommand+' diff --git ', LocalRepository, Result, Verbose);
  //FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable)+' '+GetProxyCommand+' diff ', LocalRepository, Result, Verbose);
end;

procedure THGClient.Log(var Log: TStringList);
var
  s: string = '';
begin
  FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable)+' '+GetProxyCommand+' log ', LocalRepository, s, Verbose);
  Log.Text := s;
end;

procedure THGClient.Revert;
begin
  FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable)+' '+GetProxyCommand+' revert --all --no-backup ', LocalRepository, Verbose);
end;

procedure THGClient.Update;
var
  Command: string;
begin
  // Invalidate our revision number cache
  FLocalRevision := FRET_UNKNOWN_REVISION;

  // Combined hg pull & hg update by specifying --update
  if (FDesiredRevision = '') or (trim(FDesiredRevision) = 'tip') then
    Command := ' '+GetProxyCommand+' pull --update '
  else
    Command := ' '+GetProxyCommand+' pull --update -r ' + FDesiredRevision;
  //todo: check if this desired revision works
  FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable)+Command, FLocalRepository, Verbose);
end;

procedure THGClient.ParseFileList(const CommandOutput: string;
  var FileList: TStringList; const FilterCodes: array of string);
 // Parses file lists from hg status outputs
 // If FilterCodes specified, only returns the files that match one of the characters in the code (e.g 'CGM');
 // Case-sensitive filter.
var
  AllFilesRaw: TStringList;
  Counter: integer;
  FileName: string;
  SpaceAfterStatus: integer;
  StatusCode: string;
begin
  AllFilesRaw := TStringList.Create;
  try
    AllFilesRaw.Text := CommandOutput;
    for Counter := 0 to AllFilesRaw.Count - 1 do
    begin
      //Some sample files (hg status):
      //M fpcup.ini
      //123456789
      // Also accept space in first column and entry on second column
      // Get the first character after a space in the first 2 columns:
      FileName := '';
      StatusCode := Copy(Trim(Copy(AllFilesRaw[Counter], 1, 2)), 1, 1);
      SpaceAfterStatus := PosEx(' ', AllFilesRaw[Counter], Pos(StatusCode, AllFilesRaw[Counter]));
      // Process if there is one space after the status character, and
      // we're either not filtering or we have a filter match
      if (Copy(AllFilesRaw[Counter], SpaceAfterStatus, 1) = ' ') and ((High(FilterCodes) = 0) or
        AnsiMatchStr(Statuscode, FilterCodes)) then
      begin
        FileName := (Trim(Copy(AllFilesRaw[Counter], SpaceAfterStatus, Length(AllFilesRaw[Counter]))));
        if FileName <> '' then
          FileList.Add(FileName);
      end;
    end;
  finally
    AllFilesRaw.Free;
  end;
end;

procedure THGClient.LocalModifications(var FileList: TStringList);
var
  AllFiles: TStringList;
  Output: string = '';
begin
  //quiet: hide untracked files; only show modified/added/removed/deleted files, not clean files
  FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable)+' '+GetProxyCommand+' status --modified --added --removed --deleted --quiet ',
    FLocalRepository, Output, Verbose);
  FileList.Clear;
  AllFiles := TStringList.Create;
  try
    // No filter necessary; command above already preselected relevant files
    ParseFileList(Output, AllFiles, []);
    FileList.AddStrings(AllFiles);
  finally
    AllFiles.Free;
  end;
end;

function THGClient.LocalRepositoryExists: boolean;
var
  Output: string = '';
  URL: string;
begin
  Result := false;
  //svn info=>hg summary;
  FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable)+' '+GetProxyCommand+' summary ', FLocalRepository, Output, Verbose);
  if Pos('branch:', Output) > 0 then
  begin
    // There is an hg repository here.

    // Now, repository URL might differ from the one we've set
    // Try to find out remote repo (could also have used hg paths, which gives default = https://bitbucket.org/reiniero/fpcup)
    FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable)+' '+GetProxyCommand+' showconfig paths.default ', FLocalRepository, Output, Verbose);
    if FReturnCode = 0 then
    begin
      URL := IncludeTrailingSlash(trim(Output));
    end
    else
    begin
      URL := ''; //explicitly fail
    end;

    if FRepositoryURL = '' then
    begin
      FRepositoryURL := URL;
      Result := true;
    end
    else
    begin
      if FRepositoryURL = URL then
      begin
        Result := true;
      end
      else
      begin
        // There is a repository here, but it was checked out
        // from a different URL...
        // Keep result false; show caller what's going on.
        FLocalRevision := FRET_UNKNOWN_REVISION;
        FReturnCode := FRET_LOCAL_REMOTE_URL_NOMATCH;
      end;
    end;
  end;
end;

function THGClient.GetLocalRevision: string;
const
  HashLength = 12; //12 characters in hg revision hash
var
  Output: string = '';
begin
  // Only update if we have invalid revision info, in order to minimize hg info calls
  if FLocalRevision = FRET_UNKNOWN_REVISION then
  begin
    FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable)+' '+GetProxyCommand+' identify --id ', FLocalRepository, Output, Verbose);
    if FReturnCode = 0 then
    begin
      FLocalRevision := copy(trim(Output), 1, HashLength); //ignore any + - changed working copy - at the end of the revision
    end
    else
    begin
      FLocalRevision := FRET_UNKNOWN_REVISION; //for compatibility with the svnclient code
    end;
  end;
  Result := FLocalRevision;
end;

function THGClient.GetProxyCommand: string;
begin
  if FHTTPProxyHost<>'' then
  begin
    result:='--config http_proxy.host='+FHTTPProxyHost+':'+inttostr(FHTTPProxyPort);
    if FHTTPProxyUser<>'' then
      result:=result+' --config http_proxy.user='+FHTTPProxyUser;
    if FHTTPProxyPassword<>'' then
      result:=result+' --config http_proxy.passwd='+FHTTPProxyPassword;
  end
  else
  begin
    result:='';
  end;
end;

constructor THGClient.Create;
begin
  inherited Create;
end;

destructor THGClient.Destroy;
begin
  inherited Destroy;
end;

end.
