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
  FRET_LOCAL_REMOTE_TAG_NOMATCH = repoclient.FRET_LOCAL_REMOTE_TAG_NOMATCH;
  FRET_WORKING_COPY_TOO_OLD = repoclient.FRET_WORKING_COPY_TOO_OLD;
  FRET_UNKNOWN_REVISION = repoclient.FRET_UNKNOWN_REVISION;

type
  TGitClient = class(TRepoClient)
  private
    procedure Init;
  protected
    procedure CheckOut({%H-}UseForce:boolean=false); override;
    function  GetProxyCommand: string;
    function  GetLocalRevision: string; override;
    function  GetRepoExecutable: string; override;
    function  GetRepoExecutableName: string; override;
    function  FindRepoExecutable: string; override;
    function  GetRepositoryURL:string; override;
    procedure SetDesiredTag(AValue: string); override;
  public
    procedure CheckOutOrUpdate; override;
    function Commit(Message: string): boolean; override;
    function GetDiffAll: string; override;
    procedure LocalModifications(var FileList: TStringList); override;
    function LocalRepositoryExists: boolean; override;
    procedure Log(var Log: TStringList); override;
    procedure ParseFileList(const CommandOutput: string; var FileList: TStringList; const FilterCodes: array of string); override;
    procedure Revert; override;
    procedure Update; override;
    function GetSVNRevision: string;
    function GetGitHash: string;
    function GetCommitMessage: string; override;
    function GetCommitName: string; override;
  end;

  EGitClientError = class(ERepoClientError);

implementation

uses
  Process,
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
//var
//  rv:integer;
begin
  Result := FRepoExecutable;
  // Look in path
  // Windows: will also look for <gitName>.exe
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := Which(RepoExecutableName);

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

  {$ifdef UNIX}
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := ('/usr/local/git/bin/'  + RepoExecutableName);
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := ('/opt/local/git/bin/'  + RepoExecutableName);
  {$endif UNIX}

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

function TGitClient.GetRepositoryURL:string;
var
  aURL:string;
begin
  result:=inherited;
  if (Pos('gitlab.com/freepascal.org',result)>0) then
  begin
    // To run errorfree on all systems, .git needs to be appended to the desired URL
    aURL:=ExcludeTrailingSlash(result);
    if (NOT AnsiEndsText('.git',aURL)) then aURL:=aURL+'.git';
    if (result[Length(result)]='/') then aURL:=aURL+'/';
    result:=aURL;
  end;
end;

procedure TGitClient.SetDesiredTag(AValue: string);
begin
  inherited;
  ExportOnly:=(Length(DesiredTag)>0);
end;

procedure TGitClient.CheckOut(UseForce:boolean=false);
// SVN checkout is more or less equivalent to git clone
var
  Command       : string = '';
  Output        : string = '';
  RetryAttempt  : integer;
  Branch        : string;
  //TargetFile: string;
begin
  if NOT ValidClient then exit;

  // Invalidate our revision number cache
  FLocalRevision := FRET_UNKNOWN_REVISION;

  //Command:=' clone --recurse-submodules';
  Command:=' clone';

  if ExportOnly then Command:=Command+' --depth=1';

  Branch:='';
  if (DesiredBranch<>'') then
  begin
    Branch := DesiredBranch;
  end
  else
  if (Length(DesiredTag)>0) AND (Uppercase(trim(DesiredTag)) <> 'MAIN') AND (Uppercase(trim(DesiredTag)) <> 'MASTER') then
  begin
    Branch := DesiredTag;
  end;
  if (Length(Branch)>0) then Command := Command+ ' --branch ' + Branch;

  Command := Command + ' ' +  Repository + ' ' + LocalRepository;

  FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + Command, Output, Verbose);

  if (ReturnCode=AbortedExitCode) then exit;

  if (FReturnCode<>0) then
  begin
    // If command fails, e.g. due to SSL verify error, retry with SSL-verify disabled
    // Message: "SSL certificate problem: self signed certificate in certificate chain"
    if (Pos('SSL certificate problem',Output)>0) then
    begin
      Init;
    end
    else
    begin
      RetryAttempt := 1;
      // if we have a proxy, set it now !
      if (RetryAttempt=1) then
      begin
        if Length(GetProxyCommand)>0 then TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) +  GetProxyCommand, Output, Verbose);
      end;
      while (FReturnCode <> 0) and (RetryAttempt < ERRORMAXRETRIES) do
      begin
        Sleep(500); //Give everybody a chance to relax ;)
        FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + Command, Output, Verbose); //attempt again
        if (ReturnCode=AbortedExitCode) then exit;
        Inc(RetryAttempt);
      end;
    end;
  end;

  if (FReturnCode=0) then
  begin
    // Disable detached head warning
    Command := ' config --local advice.detachedHead false';
    TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + Command, LocalRepository, Verbose);
  end;

  (*

  if (DesiredRevision<>'') then
  begin
    if ExportOnly then
    begin
      Command:= ' fetch --depth=1 origin '+ DesiredRevision;
      FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + Command, Output, Verbose);
    end;

    Command:= ' checkout '+ DesiredRevision;
    FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + Command, Output, Verbose);
  end;
  *)
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
  if NOT ValidClient then exit;
  //FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' diff --git ', LocalRepository, Result, Verbose);
  //FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' diff --no-prefix -p ', LocalRepository, Result, Verbose);
  FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' diff -p ', LocalRepository, Result, Verbose);
end;

procedure TGitClient.Log(var Log: TStringList);
var
  s: string = '';
begin
  FReturnCode := 0;
  Log.Text := s;
  if NOT ValidClient then exit;
  FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' log ', LocalRepository, s, Verbose);
  Log.Text := s;
end;

procedure TGitClient.Revert;
begin
  FReturnCode := 0;
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
  if NOT ValidClient then exit;

  // Invalidate our revision number cache
  FLocalRevision := FRET_UNKNOWN_REVISION;

  // Check if we have a remote
  FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' remote -v', LocalRepository, Output, Verbose);
  if (FReturnCode=0) then
  begin
    if (Length(Trim(Output))=0) then
    begin
      // No remote: do an init.
      // This should never happen, but anyhow.
      Init;
      exit;
    end;
  end;

  Command:='';

  if ((Length(Command)=0) AND (Length(DesiredTag)>0)) then
  begin
    FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' describe --tags --abbrev=0', LocalRepository, Output, Verbose);
    if FReturnCode = 0 then
    begin
      if (DesiredTag<>Trim(Output)) then Command := ' checkout --force '+DesiredTag;
    end;
  end;

  if (Length(Command)=0) then
  begin
    // Perhaps we also need to check that DesiredBranch is empty or "main" or FPCTRUNKBRANCH or LAZARUSTRUNKBRANCH
    if (Length(DesiredRevision)>0) AND (Uppercase(trim(DesiredRevision)) <> 'HEAD') then DesiredBranch := DesiredRevision;
    if (Length(DesiredBranch)>0) then
    begin
      FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' rev-parse --abbrev-ref HEAD', LocalRepository, Output, Verbose);
      if (FReturnCode<>0) then FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' branch --show-current', LocalRepository, Output, Verbose);
      if (FReturnCode=0) then
      begin
        if (DesiredBranch<>Trim(Output)) then Command := ' checkout --force '+DesiredBranch;
      end;
    end
  end;

  if (Length(Command)=0) then
  begin
    // Just do a simple pull
    Command := ' pull';
  end
  else
  begin
    // First do a fetch to get all the current commits
    if (NOT ExportOnly) then TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' fetch', LocalRepository, Output, Verbose);
  end;

  FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + command, LocalRepository, Output, Verbose);
  FReturnOutput := Output;
end;

procedure TGitClient.Init;
var
  Command       : string;
  Output        : string = '';
  RunOnlyTwice  : boolean;
begin
  FReturnCode := 0;
  if NOT ValidClient then exit;

  // Invalidate our revision number cache
  FLocalRevision := FRET_UNKNOWN_REVISION;

  ForceDirectoriesSafe(LocalRepository);

  Command := ' init --quiet';
  FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + Command, LocalRepository, Output, Verbose);
  // Disable detached head warning
  Command := ' config --local advice.detachedHead false';
  FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + Command, LocalRepository, Output, Verbose);

  Command := ' remote add origin '+Repository;
  FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + Command, LocalRepository, Output, Verbose);

  for RunOnlyTwice in boolean do
  begin
    Command := ' fetch --tags --quiet';
    FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + Command, LocalRepository, Output, Verbose);
    if (FReturnCode=0) then break;
    if (NOT RunOnlyTwice) then
    begin
      // If command fails, e.g. due to SSL verify error, retry with SSL-verify disabled
      // Message: "SSL certificate problem: self signed certificate in certificate chain"
      if (Pos('SSL certificate problem',Output)>0) then
      begin
        // Never disable ssl on these sites !!
        if (AnsiStartsStr('https://gitlab.com',Repository) OR AnsiStartsStr('https://github.com',Repository) OR AnsiContainsStr(Repository,'git.sourceforge.net')) then
        begin
          FReturnCode := AbortedExitCode;
          exit;
        end
        else
        begin
          Command := ' config --local http.sslVerify false';
          FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + Command, LocalRepository, Output, Verbose);
        end;
      end;
    end;
  end;

  Command := ' pull origin HEAD';
  FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + Command, LocalRepository, Output, Verbose);
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
  RepoURL,RemoteURL:string;
begin
  result := false;
  FReturnCode := 0;
  if NOT ValidClient then exit;
  if NOT DirectoryExists(LocalRepository) then exit;

  // This will output nothing to stdout and
  // fatal: Not a git repository (or any of the parent directories): .git
  // to std err
  FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' status --porcelain ', LocalRepository, Output, Verbose);
  if (FReturnCode = 0) then
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
      result := true;
    end
    else
    begin
      RepoURL:=ExcludeTrailingSlash(StripUrl(URL));
      RemoteURL:=ExcludeTrailingSlash(StripUrl(Repository));
      if (Pos(RepoURL,RemoteURL)=1) then
      begin
        result := true;
        // Check if we have an exact match.
        // If not, set remote URL to correct value.
        // This might be the case when .git is missing from the URL.
        // If things are well, this has to be done only once.
        if (RepoURL<>RemoteURL) then
        begin
          FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' remote set-url origin ' + Repository, LocalRepository, Output, Verbose);
          if (FReturnCode <> 0) then
          begin
            result := false;
          end;
        end;
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

    if result then
    begin
      if (ExportOnly AND (Length(DesiredTag)>0)) then
      begin
        // Check tag
        FReturnCode:=TInstaller(Parent).ExecuteCommandInDir(FRepoExecutable,['describe','--tags'],LocalRepository, Output, '', Verbose);
        if (FReturnCode = 0) then
        begin
          Output:=Trim(Output);
          if (Length(Output)>0) AND (Trim(Output)<>DesiredTag) then
          begin
            // There is a repository here, but with different tag
            result         := false;
            FLocalRevision := FRET_UNKNOWN_REVISION;
            FReturnCode    := FRET_LOCAL_REMOTE_TAG_NOMATCH;
            Repository     := URL;
          end;

        end;

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
  if NOT ValidClient then exit;
  if NOT DirectoryExists(LocalRepository) then exit;

  // Only update if we have invalid revision info, in order to minimize git describe calls
  if (FLocalRevision = FRET_UNKNOWN_REVISION) then
  begin
    try

      if (FLocalRevision = FRET_UNKNOWN_REVISION) then
      begin
        FReturnCode := TInstaller(Parent).ExecuteCommandInDir(FRepoExecutable,['log','-1','--pretty=format:%h'],LocalRepository, Output, '', Verbose);
        if (FReturnCode = 0) then
        begin
          FLocalRevision := trim(Output);
        end
      end;

      if (FLocalRevision = FRET_UNKNOWN_REVISION) then
      begin
        FReturnCode := TInstaller(Parent).ExecuteCommandInDir(FRepoExecutable,['describe','--tags','--long','--always'],LocalRepository, Output, '', Verbose);
        if (FReturnCode = 0) then
        begin
          // if there are any tags in this branch it will output "<tag>-<ahead>-g<hash>"
          // and if there are no tags then it will just output "<hash>",
          FLocalRevision := trim(Output);
          // Do we have this format : branchname-xxxx-gxxxx
          if (OccurrencesOfChar(FLocalRevision,'-')>=2) then
          begin
            i:=RPos('-g',FLocalRevision);
            if (i>0) then FLocalRevision:=Copy(FLocalRevision,i+2,MaxInt);
          end;
        end
      end;

      if (FLocalRevision = FRET_UNKNOWN_REVISION) then
      begin
        FReturnCode := TInstaller(Parent).ExecuteCommandInDir(FRepoExecutable,['log','-g','-1','--pretty=oneline'],LocalRepository, Output, '', Verbose);
        if (FReturnCode = 0) then
        begin
          i:=RPos(' to ',Output);
          if (i>0) then
          begin
            Delete(Output,1,(i+3));
            // Do we have this format : branchname-xxxx-gxxxx
            if (OccurrencesOfChar(Output,'-')>=2) then
              FLocalRevision := trim(Output);
          end;
        end
      end;

      if (FLocalRevision = FRET_UNKNOWN_REVISION) then
      begin
        FReturnCode := TInstaller(Parent).ExecuteCommandInDir(FRepoExecutable,['describe','--tags','--all','--long','--always'],LocalRepository, Output, '', Verbose);
        if (FReturnCode = 0) then
        begin
          i:=RPos('/',Output);
          if (i>0) then Delete(Output,1,i);
          FLocalRevision := trim(Output);
          // Do we have this format : branchname-xxxx-gxxxx
          if (OccurrencesOfChar(FLocalRevision,'-')>=2) then
          begin
            i:=RPos('-g',FLocalRevision);
            if (i>0) then FLocalRevision:=Copy(FLocalRevision,i+2,MaxInt);
          end;
        end;
      end;

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
  // As SVN revision info is not updated (anymore), disabled.
  exit;
  if NOT ValidClient then exit;
  if NOT DirectoryExists(LocalRepository) then exit;

  //Output:='';
  //i:=TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' rev-parse --show-toplevel',LocalRepository, Output, Verbose);

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

function TGitClient.GetCommitMessage: string;
var
  Output:string;
  i,j:integer;
begin
  result:='';
  if NOT ValidClient then exit;
  if NOT DirectoryExists(LocalRepository) then exit;

  Output:='';

  i:=TInstaller(Parent).ExecuteCommandInDir(FRepoExecutable,['show','-s','--format=%s%b'],LocalRepository, Output, '', Verbose);
  Output:=Trim(Output);
  if ((i=0) and (Length(Output)>0)) then
  begin
    j:=1;
    while Output[j] in [' ','*'] do
    begin
      Inc(j);
      if (j>Length(Output)) then break;
    end;
    Dec(j);
    Delete(Output,1,j);
    result:=Output;
  end;
end;

function TGitClient.GetCommitName: string;
const
  TAGMAGIC = 'tags/';
var
  Output:string;
  i,j:integer;
begin
  result:='';
  if NOT ValidClient then exit;
  if NOT DirectoryExists(LocalRepository) then exit;

  Output:='';

  FReturnCode:=TInstaller(Parent).ExecuteCommandInDir(FRepoExecutable,['describe','HEAD'],LocalRepository, Output, '', Verbose);
  if (FReturnCode<>0) OR (Length(Trim(Output))=0) then
    FReturnCode:=TInstaller(Parent).ExecuteCommandInDir(FRepoExecutable,['describe','--tags','--exact-match','HEAD'],LocalRepository, Output, '', Verbose);
  if (FReturnCode<>0) OR (Length(Trim(Output))=0) then
    FReturnCode:=TInstaller(Parent).ExecuteCommandInDir(FRepoExecutable,['describe','--tags','--always','HEAD'],LocalRepository, Output, '', Verbose);
  if (FReturnCode<>0) OR (Length(Trim(Output))=0) then
    FReturnCode:=TInstaller(Parent).ExecuteCommandInDir(FRepoExecutable,['describe','--contains','--all','HEAD'],LocalRepository, Output, '', Verbose);
  if (FReturnCode<>0) OR (Length(Trim(Output))=0) then
    FReturnCode:=TInstaller(Parent).ExecuteCommandInDir(FRepoExecutable,['name-rev','--name-only','HEAD'],LocalRepository, Output, '', Verbose);

  Output:=Trim(Output);
  j:=Pos(TAGMAGIC,Output);
  if (j<1) then j:=1 else j:=j+Length(TAGMAGIC);

  result:=Copy(Output,j,MaxInt)
end;

end.
