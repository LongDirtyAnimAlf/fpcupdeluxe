{ Classes for using mercurial/hg commands
  Copyright (C) 2012-2013 Reinier Olislagers, Ludo Brands

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

unit hgclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  processutils,
  FileUtil {Requires LCL}, repoclient;

const
  // Custom return codes
  FRET_LOCAL_REMOTE_URL_NOMATCH=repoclient.FRET_LOCAL_REMOTE_URL_NOMATCH;
  FRET_WORKING_COPY_TOO_OLD=repoclient.FRET_WORKING_COPY_TOO_OLD;
  FRET_UNKNOWN_REVISION=repoclient.FRET_UNKNOWN_REVISION;

type
  EHGClientError = class(Exception);
  { ThgClient }

  THGClient = class(TRepoClient)
  protected
    function GetLocalRevision: string; override;
    function GetRepoExecutable: string; override;
    procedure SetRepoExecutable(AValue: string); override;
  public
    procedure CheckOut; override;
    procedure CheckOutOrUpdate; override;
    function GetDiffAll:string; override;
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
uses strutils, regexpr;


{ ThgClient }
function ThgClient.FindRepoExecutable: string;
const
  // Application name:
  hgName = 'hg';
begin
  Result := FRepoExecutable;
  // Look in path
  // Windows: will also look for <hgName>.exe
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := FindDefaultExecutablePath(hgName);

{$IFDEF MSWINDOWS}
  // Some popular locations for Tortoisehg:
  // Covers both 32 bit and 64 bit Windows.
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := GetEnvironmentVariable('ProgramFiles\TorToisehg\hg.exe');
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := GetEnvironmentVariable('ProgramFiles(x86)\TorToisehg\hg.exe');
  //Directory where current executable is:
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := (ExtractFilePath(ParamStr(0)) + 'hg');
{$ENDIF MSWINDOWS}

  if not FileExists(FRepoExecutable) then
  begin
    //current directory. Note: potential for misuse by malicious program.
  {$IFDEF MSWINDOWS}
    if FileExists(hgName+'.exe') then
      FRepoExecutable := hgName+'.exe';
  {$ENDIF MSWINDOWS}
    if FileExists('hg') then
      FRepoExecutable := hgName;
  end;

  if FileExists(FRepoExecutable) then
  begin
    // Check for valid hg executable
    if ExecuteCommand(FRepoExecutable+ ' --version',Verbose) <> 0 then
    begin
      // File exists, but is not a valid hg client
      FRepoExecutable := EmptyStr;
    end;
  end
  else
  begin
    // File does not exist
    // Make sure we don't call an arbitrary executable:
    FRepoExecutable := EmptyStr;
  end;
  Result := FRepoExecutable;
end;

function ThgClient.GetRepoExecutable: string;
//todo: replace with getrepoexecutable
begin
  if not FileExists(FRepoExecutable) then FindRepoExecutable;
  if not FileExists(FRepoExecutable) then
    Result:=''
  else
    Result := FRepoExecutable;
end;

procedure Thgclient.CheckOut;
const
  MaxRetries = 3;
var
  Command: string;
  Output: string;
  RetryAttempt: integer;
begin
  // Invalidate our revision number cache
  FLocalRevision:=FRET_UNKNOWN_REVISION;

  //tip is similar to svn HEAD
  // Could add --insecure to ignore certificate problems, but rather not
  if (FDesiredRevision='') or (trim(FDesiredRevision)='tip') then
    Command := ' clone -r tip ' + Repository + ' ' + LocalRepository
  else
    Command := ' clone -r '+ FDesiredRevision+ ' ' + Repository + ' ' + LocalRepository;
  FReturnCode:=ExecuteCommand(RepoExecutable+Command,Output,Verbose);
  // If command fails, e.g. due to misconfigured firewalls blocking ICMP etc, retry a few times
  RetryAttempt := 1;
  if (ReturnCode<>0) then
  begin
    while (ReturnCode <> 0) and (RetryAttempt < MaxRetries) do
    begin
      Sleep(500); //Give everybody a chance to relax ;)
      FReturnCode:=ExecuteCommand(RepoExecutable+Command,Output,Verbose); //attempt again
      RetryAttempt := RetryAttempt + 1;
    end;
  end;
end;

procedure Thgclient.CheckOutOrUpdate;
begin
  if LocalRepositoryExists = False then
  begin
    if FReturnCode=FRET_LOCAL_REMOTE_URL_NOMATCH then
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

function ThgClient.GetDiffAll:string;
begin
  FReturnCode:=ExecuteCommandInDir(RepoExecutable+' diff --git ',LocalRepository,Result,Verbose);
end;

procedure Thgclient.Log(var Log: TStringList);
var
  s:string='';
begin
  FReturnCode:=ExecuteCommandInDir(RepoExecutable+' log ',LocalRepository,s,Verbose);
  Log.Text:=s;
end;

procedure Thgclient.Revert;
begin
  FReturnCode:=ExecuteCommandInDir(RepoExecutable+' revert --all --no-backup ',LocalRepository,Verbose);
end;

procedure ThgClient.SetRepoExecutable(AValue: string);
begin
  if FRepoExecutable <> AValue then
  begin
    FRepoExecutable := AValue;
    FindRepoExecutable; //Make sure it actually exists; use fallbacks if possible
  end;
end;

procedure Thgclient.Update;
const
  MaxErrorRetries = 3;
  MaxUpdateRetries = 9;
var
  Command: string;
  AfterErrorRetry: integer; // Keeps track of retry attempts after error result
  UpdateRetry: integer; // Keeps track of retry attempts to get all files
begin
  AfterErrorRetry := 1;
  UpdateRetry := 1;

  // Invalidate our revision number cache
  FLocalRevision:=FRET_UNKNOWN_REVISION;

  // Combined hg pull & hg update by specifying --update
  if (FDesiredRevision='') or (trim(FDesiredRevision)='tip') then
    Command := ' pull --update '
  else
    Command := ' pull --update -r ' + FDesiredRevision;
//todo: check if this desired revision works
  FReturnCode:=ExecuteCommandInDir(RepoExecutable+command,FLocalRepository,Verbose);
end;

procedure ThgClient.ParseFileList(const CommandOutput: string; var FileList: TStringList; const FilterCodes: array of string);
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
  AllFilesRaw:=TStringList.Create;
  try
    AllFilesRaw.Text := CommandOutput;
    for Counter := 0 to AllFilesRaw.Count - 1 do
    begin
      //Some sample files (hg status):
      //M fpcup.ini
      //123456789
      // Also accept space in first column and entry on second column
      // Get the first character after a space in the first 2 columns:
      FileName:='';
      StatusCode:=Copy(Trim(Copy(AllFilesRaw[Counter],1,2)),1,1);
      SpaceAfterStatus:=PosEx(' ', AllFilesRaw[Counter], Pos(StatusCode, AllFilesRaw[Counter]));
      // Process if there is one space after the status character, and
      // we're either not filtering or we have a filter match
      if (Copy(AllFilesRaw[Counter], SpaceAfterStatus,1)=' ') and
        ((High(FilterCodes)=0) or AnsiMatchStr(Statuscode, FilterCodes)) then
      begin
        FileName:=(Trim(Copy(AllFilesRaw[Counter],SpaceAfterStatus,Length(AllFilesRaw[Counter]))));
        if FileName<>'' then FileList.Add(FileName);
      end;
    end;
  finally
    AllFilesRaw.Free;
  end;
end;

procedure ThgClient.LocalModifications(var FileList: TStringList);
var
  AllFiles: TStringList;
  Output: string='';
begin
  //quiet: hide untracked files; only show modified/added/removed/deleted files, not clean files
  FReturnCode:=ExecuteCommandInDir(RepoExecutable+' status --modified --added --removed --deleted --quiet ',FLocalRepository,Output,Verbose);
  FileList.Clear;
  AllFiles:=TStringList.Create;
  try
    // No filter necessary; command above already preselected relevant files
    ParseFileList(Output, AllFiles, []);
    FileList.AddStrings(AllFiles);
  finally
    AllFiles.Free;
  end;
end;

function Thgclient.LocalRepositoryExists: boolean;
const
  URLLen=Length('URL: ');
var
  Output:string='';
  URL: string;
  URLPos: integer;
begin
  Result := False;
  //svn info=>hg summary;
  FReturnCode := ExecuteCommandInDir(RepoExecutable+' summary ',FLocalRepository,Output,Verbose);
  if Pos('branch:', Output) > 0 then
  begin
    // There is an hg repository here.

    // Now, repository URL might differ from the one we've set
    // Try to find out remote repo (could also have used hg paths, which gives default = https://bitbucket.org/reiniero/fpcup)
    FReturnCode := ExecuteCommandInDir(RepoExecutable+' showconfig paths.default ',FLocalRepository,Output,Verbose);
    if FReturnCode=0 then
    begin
      URL:=IncludeTrailingSlash(trim(Output)); //todo: check trailing slash
    end
    else
    begin
      URL:=''; //explicitly fail
    end;

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
        FLocalRevision:=FRET_UNKNOWN_REVISION;
        FReturnCode:=FRET_LOCAL_REMOTE_URL_NOMATCH;
      end;
    end;
  end;
end;

function ThgClient.GetLocalRevision: string;
const
  HashLength=12; //12 characters in hg revision hash
var
  Output: string;
begin
  // Only update if we have invalid revision info, in order to minimize hg info calls
  if FLocalRevision=FRET_UNKNOWN_REVISION then
  begin
    FReturnCode:=ExecuteCommandInDir(RepoExecutable+' identify --id ',FLocalRepository,Output,Verbose);
    if FReturnCode=0 then
    begin
      FLocalRevision:=copy(trim(Output),1,HashLength); //ignore any + - changed working copy - at the end of the revision
    end
    else
    begin
      FLocalRevision:=FRET_UNKNOWN_REVISION; //for compatibility with the svnclient code
    end;
  end;
  result:=FLocalRevision;
end;

constructor Thgclient.Create;
begin
  inherited Create;
end;

destructor Thgclient.Destroy;
begin
  inherited Destroy;
end;
end.
