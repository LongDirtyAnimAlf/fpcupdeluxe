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
  repoclient;

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
    function GetProxyCommand: string;
    function GetLocalRevision: string; override;
    function GetRepoExecutable: string; override;
    function GetRepoExecutableName: string; override;
    function FindRepoExecutable: string; override;
  public
    procedure CheckOutOrUpdate; override;
    function Commit(Message: string): boolean; override;
    function GetDiffAll: string; override;
    procedure SwitchURL; override;
    procedure LocalModifications(var FileList: TStringList); override;
    function LocalRepositoryExists: boolean; override;
    procedure Log(var Log: TStringList); override;
    procedure ParseFileList(const CommandOutput: string; var FileList: TStringList; const FilterCodes: array of string); override;
    procedure Revert; override;
    procedure Update; override;
    function GetSVNRevision: string;
    function GetGitHash: string;
  end;

implementation

uses
  StrUtils,
  installerCore,
  processutils,
  fpcuputil;

{ TGitClient }
function TGitClient.GetRepoExecutableName: string;
begin
  // Application name:
  result := 'git';
end;

function TGitClient.FindRepoExecutable: string;
var
  rv:integer;
begin
  Result := FRepoExecutable;
  // Look in path
  // Windows: will also look for <gitName>.exe
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := Which(RepoExecutableName+GetExeExt);

  {$IFDEF MSWINDOWS}
  // Git on Windows can be a .cmd file
  //if not FileExists(FRepoExecutable) then
  //  FRepoExecutable := FindDefaultExecutablePath(RepoExecutableName + '.cmd');

  // Git installed via msyswin
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := 'C:\msysgit\bin\' + RepoExecutableName + '.exe';
  // Some popular locations for Tortoisegit:
  // Covers both 32 bit and 64 bit Windows.
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := GetEnvironmentVariable('ProgramFiles')+'\TortoiseGit\bin\' + RepoExecutableName + '.exe';
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := GetEnvironmentVariable('ProgramFiles(x86)')+'\TortoiseGit\bin\' + RepoExecutableName + '.exe';
  // Commandline git tools
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := GetEnvironmentVariable('ProgramFiles')+'\Git\bin\' + RepoExecutableName + '.exe';
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := GetEnvironmentVariable('ProgramFiles(x86)')+'\Git\bin\' + RepoExecutableName + '.exe';
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
    //rv:=TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + ' --version', Verbose);
    //if rv<>0 then
    if (NOT CheckExecutable(FRepoExecutable, ['--version'], '', true)) then
    begin
      FRepoExecutable := '';
      //ThreadLog('GIT client found, but error code during check: '+InttoStr(rv),etError);
      ThreadLog('GIT client found, but error code during check !',etError);
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
    FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + Command, Verbose);
    FReturnCode := TInstaller(Parent).ExecuteCommand(FUnzip+' -o -d '+IncludeTrailingPathDelimiter(InstallDir)+' '+TargetFile,Verbose);
    SysUtils.DeleteFile(TargetFile);
    }
    if DirectoryExists(IncludeTrailingPathDelimiter(LocalRepository)+'.git') then
    begin
      Command:=DoubleQuoteIfNeeded(FRepoExecutable) + ' fetch --all';
      TInstaller(Parent).ExecuteCommandInDir(Command, LocalRepository, Verbose);
      if (DesiredBranch<>'') then
        Command:=DoubleQuoteIfNeeded(FRepoExecutable) + ' reset --hard origin/'+DesiredBranch
      else
        Command:=DoubleQuoteIfNeeded(FRepoExecutable) + ' reset --hard';
      TInstaller(Parent).ExecuteCommandInDir(Command, LocalRepository, Verbose);
      Command:='';
    end
    else
    begin
      // initial : very shallow clone = fast !!
      Command := ' clone --recurse-submodules --depth 1';
      if (DesiredBranch<>'') then
        Command := Command +' -b ' + DesiredBranch;

    end;
  end
  else
  begin
    //On Haiku, arm and aarch64, always get a shallow copy of the repo
    {$if defined(CPUAARCH64) OR defined(CPUARM) OR (defined(CPUPOWERPC64) AND defined(FPC_ABI_ELFV2)) OR defined(Haiku) OR defined(AROS) OR defined(Morphos)}
    Command := ' clone --recurse-submodules --depth 1';
    {$else}
    Command := ' clone --recurse-submodules';
    {$endif}
    if (DesiredBranch<>'') then
      Command := Command +' -b ' + DesiredBranch;
  end;

  if (Command<>'') then
  begin
    if (Length(DesiredRevision)>0) AND (Uppercase(trim(DesiredRevision)) <> 'HEAD') then
      Command := Command+ ' ' + DesiredRevision;

    if (Length(DesiredTag)>0) AND (Uppercase(trim(DesiredTag)) <> 'MAIN') AND (Uppercase(trim(DesiredTag)) <> 'MASTER') then
      Command := Command+ ' --depth 1 --branch ' + DesiredTag;

    Command := Command + ' ' +  Repository + ' ' + LocalRepository;

    FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + Command, Output, Verbose);
    FReturnOutput := Output;
  end
  else FReturnCode := 0;

  if (ReturnCode=AbortedExitCode) then exit;

  // If command fails, e.g. due to misconfigured firewalls blocking ICMP etc, retry a few times
  RetryAttempt := 1;
  if (FReturnCode <> 0) then
  begin
    // if we have a proxy, set it now !
    if Length(GetProxyCommand)>0 then TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) +  GetProxyCommand, Output, Verbose);
    while (FReturnCode <> 0) and (RetryAttempt < ERRORMAXRETRIES) do
    begin
      Sleep(500); //Give everybody a chance to relax ;)
      FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + Command, Output, Verbose); //attempt again
      if (ReturnCode=AbortedExitCode) then exit;
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
      if (FReturnCode<>AbortedExitCode) then Update;
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
  FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' commit --message='+Message, LocalRepository, Verbose);
  //todo: do push to remote repo?
  Result:=(FReturnCode=0);
end;

function TGitClient.GetDiffAll: string;
begin
  result:='';
  FReturnCode := 0;
  if ExportOnly then exit;
  if NOT ValidClient then exit;
  //FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' diff --git ', LocalRepository, Result, Verbose);
  FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' diff --no-prefix -p ', LocalRepository, Result, Verbose);
end;

procedure TGitClient.Log(var Log: TStringList);
var
  s: string = '';
begin
  FReturnCode := 0;
  Log.Text := s;
  if ExportOnly then exit;
  if NOT ValidClient then exit;
  FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' log ', LocalRepository, s, Verbose);
  Log.Text := s;
end;

procedure TGitClient.Revert;
begin
  FReturnCode := 0;
  if ExportOnly then exit;
  if NOT ValidClient then exit;
  //FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' revert --all --no-backup ', LocalRepository, Verbose);
  FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' reset --hard ', LocalRepository, Verbose);
end;

procedure TGitClient.Update;
var
  Command: string;
  Output: string = '';
begin
  FReturnCode := 0;
  if ExportOnly then exit;
  if NOT ValidClient then exit;

  // Invalidate our revision number cache
  FLocalRevision := FRET_UNKNOWN_REVISION;

  // Get updates (equivalent to git fetch and git merge)
  // --all: fetch all remotes
  Command := ' pull --all --recurse-submodules=yes';
  FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + command, LocalRepository, Output, Verbose);
  FReturnOutput := Output;

  if FReturnCode = 0 then
  begin
    // Notice that the result of a merge will not be checked out in the submodule,
    //"git submodule update" has to be called afterwards to bring the work tree up to date with the merge result.
    Command := ' submodule update ';
    FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + command, LocalRepository, Verbose);
  end;

  if (FReturnCode = 0){ and (Length(DesiredRevision)>0) and (uppercase(trim(DesiredRevision)) <> 'HEAD')}
  then
  begin
    //SSL Certificate problem
    //git config --system http.sslCAPath /absolute/path/to/git/certificates
    // always reset hard towards desired revision
    Command := ' reset --hard ' + DesiredRevision;
    FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + command, LocalRepository, Verbose);
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

procedure TGitClient.SwitchURL;
var
  Command: string = '';
  Output: string = '';
  RetryAttempt: integer;
begin
  FReturnCode := 0;
  if ExportOnly then exit;
  if NOT ValidClient then exit;

  // Actual clone/checkout
  Command := ' remote set-url origin ' +  Repository + ' ' + LocalRepository;
  FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + Command, Output, Verbose);

  // If command fails, e.g. due to misconfigured firewalls blocking ICMP etc, retry a few times
  RetryAttempt := 1;
  if (FReturnCode <> 0) then
  begin
    // if we have a proxy, set it now !
    if Length(GetProxyCommand)>0 then TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) +  GetProxyCommand, Output, Verbose);
    while (FReturnCode <> 0) and (RetryAttempt < ERRORMAXRETRIES) do
    begin
      Sleep(500); //Give everybody a chance to relax ;)
      FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + Command, Output, Verbose); //attempt again
      RetryAttempt := RetryAttempt + 1;
    end;
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
  if NOT DirectoryExists(LocalRepository) then exit;
  if (NOT Assigned(FileList)) then exit;

  FileList.Clear;
  // --porcelain indicate stable output;
  // -z would indicate machine-parsable output but uses ascii 0 to terminate strings, which doesn't match ParseFileList;
  FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' status --porcelain --untracked-files=no ',
    LocalRepository, Output, Verbose);
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
  if NOT DirectoryExists(LocalRepository) then exit;

  // This will output nothing to stdout and
  // fatal: Not a git repository (or any of the parent directories): .git
  // to std err
  FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' status --porcelain ', LocalRepository, Output, Verbose);
  if FReturnCode = 0 then
  begin
    // There is a git repository here.

    // Now, repository URL might differ from the one we've set
    // Try to find out remote repo
    FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' config remote.origin.url ', LocalRepository, Output, Verbose);
    if FReturnCode = 0 then
    begin
      URL := IncludeTrailingSlash(trim(Output));
    end
    else
    begin
      URL := ''; //explicitly fail
    end;

    if Repository = '' then
    begin
      Repository := URL;
      Result := true;
    end
    else
    begin
      if StripUrl(Repository) = StripUrl(URL) then
      begin
        Result := true;
      end
      else
      begin
        // There is a repository here, but it was checked out
        // from a different URL...
        // Keep result false; show caller what's going on.
        FLocalRevision          := FRET_UNKNOWN_REVISION;
        //FLocalRevisionWholeRepo := FRET_UNKNOWN_REVISION;
        FReturnCode             := FRET_LOCAL_REMOTE_URL_NOMATCH;
        Repository              := URL;
      end;
    end;
  end;
end;

function TGitClient.GetProxyCommand: string;
var
  s:string;
begin
  if HTTPProxyHost<>'' then
  begin
    s:=HTTPProxyHost;
    if Pos('http',s)<>1 then s:='https://'+s;
    if HTTPProxyPort<>0 then s:=s+':'+IntToStr(HTTPProxyPort);
    if HTTPProxyUser<>'' then
    begin
      s:='@'+s;
      if HTTPProxyPassword<>'' then s:=':'+HTTPProxyPassword+s;
      s:=HTTPProxyUser+s;
    end;
    result:=' config --local --add http.proxy '+s;
  end
  else
  begin
    result:='';
  end;
end;



function TGitClient.GetLocalRevision: string;
var
  Output: string = '';
  i:integer;
begin
  Result := Output;
  FReturnCode := 0;
  if ExportOnly then exit;
  if NOT ValidClient then exit;
  if NOT DirectoryExists(LocalRepository) then exit;

  // Only update if we have invalid revision info, in order to minimize git info calls
  if FLocalRevision = FRET_UNKNOWN_REVISION then
  begin
    //todo: find out: without max-count, I can get multiple entries. No idea what these mean!??
    // alternative command: rev-parse --verify "HEAD^0" but that doesn't look as low-level ;)
    try
      //FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' rev-list --max-count=1 HEAD ', LocalRepository, Output, Verbose);
      FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' describe --tags --all --long --always ', LocalRepository, Output, Verbose);
      if FReturnCode = 0 then
      begin
        i:=RPos('/',Output);
        if (i>0) then Delete(Output,1,i);
        FLocalRevision := trim(Output)
      end
        else FLocalRevision := FRET_UNKNOWN_REVISION; //for compatibility with the svnclient code
    except
      FLocalRevision := FRET_UNKNOWN_REVISION; //for compatibility with the svnclient code
    end;
  end;
  Result := FLocalRevision;
end;

function TGitClient.GetSVNRevision: string;
var
  Output:string;
  OutputSL:TStringList;
  i,j:integer;
begin
  result:='';

  if ExportOnly then exit;
  if NOT ValidClient then exit;
  if NOT DirectoryExists(LocalRepository) then exit;

  Output:='';
  i:=TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' log -n 1 --grep=^git-svn-id:',LocalRepository, Output, Verbose);
  if (i=0) then
  begin
    OutputSL:=TStringList.Create;
    try
      OutputSL.Text:=Output;
      for i:=0 to (OutputSL.Count-1) do
      begin
        Output:=Trim(OutputSL.Strings[i]);
        if Pos('git-svn-id:',Output)>0 then
        begin
          j:=Pos('@',Output);
          if (j>0) then
          begin
            Delete(Output,1,j);
            j:=Pos(' ',Output);
            if (j>0) then
            begin
              Delete(Output,j,MaxInt);
              result:=Trim(Output);
            end;
          end;
          break;
        end;
      end;
    finally
      OutputSL.Free;
    end;
  end;
end;

function TGitClient.GetGitHash: string;
var
  Output:string;
  i,j:integer;
begin
  result:='';

  if ExportOnly then exit;
  if NOT ValidClient then exit;
  if NOT DirectoryExists(LocalRepository) then exit;

  if (Length(DesiredRevision)=0) OR (Uppercase(trim(DesiredRevision)) = 'HEAD') then exit;

  Output:='';
  i:=TInstaller(Parent).ExecuteCommandInDir(FRepoExecutable,['log','--all','--grep=@'+DesiredRevision,'--pretty=oneline'],LocalRepository, Output, '', Verbose);
  if (i=0) then
  begin
    j:=Pos(' ',Output);
    if (j>0) then
    begin
      Delete(Output,j,MaxInt);
      result:=Output;
    end;
  end;
end;


end.
