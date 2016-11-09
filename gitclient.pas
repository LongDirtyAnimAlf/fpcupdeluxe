{ Classes for using git commands, based on git and svn classes
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

unit gitclient;

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
  EGitClientError = class(ERepoClientError);
  { TGitClient }

  TGitClient = class(TRepoClient)
  { Support for http proxy via git config, e.g.
  git config --global http.proxy $http_proxy
  however, we're not writing config changes for users, so
  we don't provide http proxy support for git. }
  protected
    procedure CheckOut(UseForce:boolean=false); override;
    function GetLocalRevision: string; override;
    function GetRepoExecutable: string; override;
    function GetRepoExecutableName: string; override;
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
    procedure Update; override;
    constructor Create;
    destructor Destroy; override;
  end;


implementation

uses
  fpcuputil,
  strutils;

{ TGitClient }
function TGitClient.GetRepoExecutableName: string;
begin
  // Application name:
  result := 'git';
end;


function TGitClient.FindRepoExecutable: string;
begin
  Result := FRepoExecutable;
  // Look in path
  // Windows: will also look for <gitName>.exe
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := FindDefaultExecutablePath(RepoExecutableName);


{$IFDEF MSWINDOWS}
  // Git on Windows can be a .cmd file
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := FindDefaultExecutablePath(RepoExecutableName + '.cmd');
  // Git installed via msyswin
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := 'C:\msysgit\bin\' + RepoExecutableName + '.exe';
  // Some popular locations for Tortoisegit:
  // Covers both 32 bit and 64 bit Windows.
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := GetEnvironmentVariable('ProgramFiles\TortoiseGit\bin\' + RepoExecutableName + '.exe');
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := GetEnvironmentVariable('ProgramFiles(x86)\TortoiseGit\bin\' + RepoExecutableName + '.exe');
  // Commandline git tools
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := GetEnvironmentVariable('ProgramFiles\Git\bin\' + RepoExecutableName + '.exe');
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := GetEnvironmentVariable('ProgramFiles(x86)\Git\bin\' + RepoExecutableName + '.exe');
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := 'C:\Program Files (x86)\Git\bin\' + RepoExecutableName + '.exe';

  //Directory where current executable is:
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := (SafeGetApplicationPath  + RepoExecutableName + '.exe');
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
    // Check for valid git executable:
    if ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + ' --version', Verbose) <> 0 then
    begin
      // File exists, but is not a valid git client
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

function TGitClient.GetRepoExecutable: string;
begin
  if not FileExists(FRepoExecutable) then
    FindRepoExecutable;
  if not FileExists(FRepoExecutable) then
    Result := ''
  else
    Result := FRepoExecutable;
end;

procedure TGitClient.CheckOut(UseForce:boolean=false);
// SVN checkout is more or less equivalent to git clone
const
  MaxRetries = 3;
var
  Command: string = '';
  Output: string = '';
  RetryAttempt: integer;
  //TargetFile: string;
begin
  if NOT ValidClient then exit;

  // Invalidate our revision number cache
  FLocalRevision := FRET_UNKNOWN_REVISION;

  // Actual clone/checkout
  if ExportOnly then
  begin
    {
    TargetFile := SysUtils.GetTempFileName;
    Command := ' archive --format zip --output ' + TargetFile + ' --prefix=/ --remote=' + Repository + ' master';
    FReturnCode := ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + Command, FVerbose);
    FReturnCode := ExecuteCommand(FUnzip+' -o -d '+IncludeTrailingPathDelimiter(InstallDir)+' '+TargetFile,FVerbose);
    SysUtils.DeleteFile(TargetFile);
    }
    if DirectoryExists(IncludeTrailingPathDelimiter(LocalRepository)+'.git') then
    begin
      ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' fetch --all ',LocalRepository, FVerbose);
      ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' reset --hard origin/master ',LocalRepository, FVerbose);
    end
    else
    begin
      // initial : very shallow clone = fast !!
      Command := ' clone --recurse-submodules --depth 1 ' + Repository + ' ' + LocalRepository
    end;
  end
  else
  begin
    Command := ' clone --recurse-submodules ' + Repository + ' ' + LocalRepository;
  end;

  if Command<>''
     then FReturnCode := ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + Command, Output, FVerbose)
     else FReturnCode := 0;

  // If command fails, e.g. due to misconfigured firewalls blocking ICMP etc, retry a few times
  RetryAttempt := 1;
  if (FReturnCode <> 0) then
  begin
    while (FReturnCode <> 0) and (RetryAttempt < MaxRetries) do
    begin
      Sleep(500); //Give everybody a chance to relax ;)
      FReturnCode := ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + Command, Output, FVerbose); //attempt again
      RetryAttempt := RetryAttempt + 1;
    end;
  end;
end;

procedure TGitClient.CheckOutOrUpdate;
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
      // If we use a desired revision, we'll need to update to that. Doesn't hurt anyway to run this command
      Update;
    end;
  end
  else
  begin
    // Update
    Update;
  end;
end;

function TGitClient.Commit(Message: string): boolean;
begin
  result:=false;
  FReturnCode := 0;
  if ExportOnly then exit;
  if NOT ValidClient then exit;
  inherited Commit(Message);
  FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' commit --message='+Message, LocalRepository, Verbose);
  //todo: do push to remote repo?
  Result:=(FReturnCode=0);
end;

function TGitClient.Execute(Command: string): integer;
begin
  Result:=-1;
  FReturnCode := 0;
  if NOT ValidClient then exit;
  if ExportOnly then exit;
  FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' '+Command, LocalRepository, Verbose);
  Result:= FReturnCode;
end;

function TGitClient.GetDiffAll: string;
begin
  result:='';
  FReturnCode := 0;
  if ExportOnly then exit;
  if NOT ValidClient then exit;
  //FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' diff --git ', LocalRepository, Result, Verbose);
  FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' diff -p ', LocalRepository, Result, Verbose);
end;

procedure TGitClient.Log(var Log: TStringList);
var
  s: string = '';
begin
  FReturnCode := 0;
  Log.Text := s;
  if ExportOnly then exit;
  if NOT ValidClient then exit;
  FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' log ', LocalRepository, s, Verbose);
  Log.Text := s;
end;

procedure TGitClient.Revert;
begin
  FReturnCode := 0;
  if ExportOnly then exit;
  if NOT ValidClient then exit;
  //FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' revert --all --no-backup ', LocalRepository, Verbose);
  FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' reset --hard ', LocalRepository, Verbose);
end;

procedure TGitClient.Update;
var
  Command: string;
begin
  FReturnCode := 0;
  if ExportOnly then exit;
  if NOT ValidClient then exit;

  // Invalidate our revision number cache
  FLocalRevision := FRET_UNKNOWN_REVISION;

  // Get updates (equivalent to git fetch and git merge)
  // --all: fetch all remotes
  Command := ' pull --all --recurse-submodules=yes';
  FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + command, FLocalRepository, Verbose);

  if FReturnCode = 0 then
  begin
    // Notice that the result of a merge will not be checked out in the submodule,
    //"git submodule update" has to be called afterwards to bring the work tree up to date with the merge result.
    Command := ' submodule update ';
    FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + command, FLocalRepository, Verbose);
  end;

  if (FReturnCode = 0) and (FDesiredRevision <> '') and (uppercase(trim(FDesiredRevision)) = 'HEAD') then
  begin
    // If user wants a certain revision, move back to it:
    //todo: check if this desired revision works
    Command := ' reset --hard ' + FDesiredRevision;
    FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + command, FLocalRepository, Verbose);
  end;
end;

procedure TGitClient.ParseFileList(const CommandOutput: string; var FileList: TStringList; const FilterCodes: array of string);
 // Parses file lists from git status outputs
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
      { Output like (first column is a space)
       D Aircraft/Socata-ST10/Models/Interior/Panel/Instruments/Switch/Switch.ac
       M README
      }
      // Accept space in first column and entry on second column
      // Get the first character after a space in the first 2 columns:
      FileName := '';
      StatusCode := Copy(Trim(Copy(AllFilesRaw[Counter], 1, 2)), 1, 1);
      SpaceAfterStatus := PosEx(' ', AllFilesRaw[Counter], Pos(StatusCode, AllFilesRaw[Counter]));
      // Process if there is one space after the status character, and
      // we're either not filtering or we have a filter match
      if (Copy(AllFilesRaw[Counter], SpaceAfterStatus, 1) = ' ') and ((High(FilterCodes) = 0) or
        AnsiMatchStr(Statuscode, FilterCodes)) then
      begin
        // Replace / with \ for Windows:
        FileName := (Trim(Copy(AllFilesRaw[Counter], SpaceAfterStatus, Length(AllFilesRaw[Counter]))));
        FileName := StringReplace(FileName, '/', DirectorySeparator, [rfReplaceAll]);
        if FileName <> '' then
          FileList.Add(FileName);
      end;
    end;
  finally
    AllFilesRaw.Free;
  end;
end;

procedure TGitClient.LocalModifications(var FileList: TStringList);
var
  AllFiles: TStringList;
  Output: string = '';
begin
  FReturnCode := 0;
  if ExportOnly then exit;
  if NOT ValidClient then exit;

  FileList.Clear;
  if NOT ValidClient then exit;
  // --porcelain indicate stable output;
  // -z would indicate machine-parsable output but uses ascii 0 to terminate strings, which doesn't match ParseFileList;
  FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' status --porcelain --untracked-files=no ',
    FLocalRepository, Output, Verbose);
  AllFiles := TStringList.Create;
  try
    // Modified, Added, Deleted, Renamed
    ParseFileList(Output, AllFiles, ['M', 'A', 'D', 'R']);
    FileList.AddStrings(AllFiles);
  finally
    AllFiles.Free;
  end;
end;

function TGitClient.LocalRepositoryExists: boolean;
var
  Output: string = '';
  URL: string;
begin
  Result := false;
  FReturnCode := 0;
  if ExportOnly then exit;
  if NOT ValidClient then exit;

  // This will output nothing to stdout and
  // fatal: Not a git repository (or any of the parent directories): .git
  // to std err
  FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' status --porcelain ', FLocalRepository, Output, Verbose);
  if FReturnCode = 0 then
  begin
    // There is a git repository here.

    // Now, repository URL might differ from the one we've set
    // Try to find out remote repo
    FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' config remote.origin.url ', FLocalRepository, Output, Verbose);
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

function TGitClient.GetLocalRevision: string;
var
  Output: string = '';
begin
  Result := Output;
  FReturnCode := 0;
  if ExportOnly then exit;
  if NOT ValidClient then exit;

  // Only update if we have invalid revision info, in order to minimize git info calls
  if FLocalRevision = FRET_UNKNOWN_REVISION then
  begin
    //todo: find out: without max-count, I can get multiple entries. No idea what these mean!??
    // alternative command: rev-parse --verify "HEAD^0" but that doesn't look as low-level ;)
    FReturnCode := ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' rev-list --max-count=1 HEAD ', FLocalRepository, Output, Verbose);
    if FReturnCode = 0 then
    begin
      FLocalRevision := trim(Output);
    end
    else
    begin
      FLocalRevision := FRET_UNKNOWN_REVISION; //for compatibility with the svnclient code
    end;
  end;
  Result := FLocalRevision;
end;

constructor TGitClient.Create;
begin
  inherited Create;
end;

destructor TGitClient.Destroy;
begin
  inherited Destroy;
end;

end.
