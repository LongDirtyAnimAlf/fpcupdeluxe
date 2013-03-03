{ Classes for using svn commands
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
unit svnclient;

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
  ESVNClientError = class(ERepoClientError);
  { TSVNClient }

  TSVNClient = class(TRepoClient)
  protected
    FLocalRevisionWholeRepo: string;
    procedure CheckOut; override;
    function GetLocalRevision: string; override;
    function GetLocalRevisionWholeRepo: string;
    procedure GetLocalRevisions;
    function GetRepoExecutable: string; override;
    procedure Update; override;
  public
    procedure CheckOutOrUpdate; override;
    function FindRepoExecutable: string; override;
    function GetDiffAll:string; override;
    procedure LocalModifications(var FileList: TStringList); override;
    function LocalRepositoryExists: boolean; override;
    //Revision number of local repository - the repository wide revision number regardless of what branch we are in
    property LocalRevisionWholeRepo: string read GetLocalRevisionWholeRepo;
    procedure Log(var Log: TStringList); override;
    procedure ParseFileList(const CommandOutput: string; var FileList: TStringList; const FilterCodes: array of string); override;
    procedure Revert; override;
    constructor Create;
    destructor Destroy; override;
  end;


implementation
uses strutils, regexpr;


{ TSVNClient }
function TSVNClient.FindRepoExecutable: string;
const
  // Application name:
  SVNName = 'svn';
begin
  Result := FRepoExecutable;
  // Look in path
  // Windows: will also look for <SVNName>.exe
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := FindDefaultExecutablePath(SVNName);

{$IFDEF MSWINDOWS}
  // Some popular locations for SlikSVN, Subversion, and TortoiseSVN:
  // Covers both 32 bit and 64 bit Windows.
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := GetEnvironmentVariable('ProgramFiles\Subversion\bin\svn.exe');
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := GetEnvironmentVariable('ProgramFiles(x86)\Subversion\bin\svn.exe');
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := GetEnvironmentVariable('ProgramFiles\SlikSvn\bin\svn.exe');
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := GetEnvironmentVariable('ProgramFiles(x86)\SlikSvn\bin\svn.exe');
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := GetEnvironmentVariable('ProgramFiles\TorToiseSVN\bin\svn.exe');
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := GetEnvironmentVariable('ProgramFiles(x86)\TorToiseSVN\bin\svn.exe');
  //Directory where current executable is:
  if not FileExists(FRepoExecutable) then
    FRepoExecutable := (ExtractFilePath(ParamStr(0)) + 'svn');
{$ENDIF MSWINDOWS}

  if not FileExists(FRepoExecutable) then
  begin
    //current directory. Note: potential for misuse by malicious program.
    if FileExists(SVNName+'.exe') then
      FRepoExecutable := SVNName+'.exe';
    if FileExists('svn') then
      FRepoExecutable := SVNName;
  end;

  if FileExists(FRepoExecutable) then
  begin
    // Check for valid svn executable; note on Windows we may need quoting for paths with spaces
    {$IFDEF MSWINDOWS}
    if ExecuteCommand('"'+FRepoExecutable + '" --version',Verbose) <> 0 then
    {$ELSE}
    if ExecuteCommand(FRepoExecutable+ ' --version',Verbose) <> 0 then
    {$ENDIF}
    begin
      // File exists, but is not a valid svn client
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

function TSVNClient.GetRepoExecutable: string;
begin
  if not FileExists(FRepoExecutable) then FindRepoExecutable;
  if not FileExists(FRepoExecutable) then
    Result:=''
  else
    Result := FRepoExecutable;
end;



procedure Tsvnclient.Checkout;
const
  MaxRetries = 3;
var
  Command: string;
  Output: string;
  RetryAttempt: integer;
begin
  // Invalidate our revision number cache
  FLocalRevision:=FRET_UNKNOWN_REVISION;
  FLocalRevisionWholeRepo:=FRET_UNKNOWN_REVISION;

  if (FDesiredRevision='') or (trim(FDesiredRevision)='HEAD') then
    Command := ' checkout --non-interactive -r HEAD ' + Repository + ' ' + LocalRepository
  else
    Command := ' checkout --non-interactive -r '+ FDesiredRevision+ ' ' + Repository + ' ' + LocalRepository;
  FReturnCode:=ExecuteCommand(FRepoExecutable+Command,Output,Verbose);
  // If command fails, e.g. due to misconfigured firewalls blocking ICMP etc, retry a few times
  RetryAttempt := 1;
  if (ReturnCode<>0) then
  begin
    while (ReturnCode <> 0) and (RetryAttempt < MaxRetries) do
    begin
      if Pos('E155004',Output)>0 then
      {
      E155004: Working copy '<directory>' locked.
      run 'svn cleanup' to remove locks (type 'svn help cleanup' for details)
      }
      begin
        // Let's try one time to fix it.
        FReturnCode:=ExecuteCommand(FRepoExecutable+' cleanup --non-interactive '+ LocalRepository,Verbose); //attempt again
        // We probably ended up with a local repository where not all files were checked out.
        // Let's call update to do so.
        Update;
      end;
      Sleep(500); //Give everybody a chance to relax ;)
      FReturnCode:=ExecuteCommand(FRepoExecutable+Command,Output,Verbose); //attempt again
      RetryAttempt := RetryAttempt + 1;
    end;
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
      // Just to be sure, update as well (checkout may have failed without warning):
      Update;
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
  FReturnCode:=ExecuteCommand(FRepoExecutable+' diff ' + LocalRepository,Result,Verbose);
end;

procedure Tsvnclient.Log(var Log: TStringList);
var
  s:string='';
begin
  FReturnCode:=ExecuteCommand(FRepoExecutable+' log ' + LocalRepository,s,Verbose);
  Log.Text:=s;
end;

procedure Tsvnclient.Revert;
begin
  FReturnCode:=ExecuteCommand(FRepoExecutable+' revert --recursive ' + LocalRepository,Verbose);
end;

procedure Tsvnclient.Update;
const
  MaxErrorRetries = 3;
  MaxUpdateRetries = 9;
var
  Command: string;
  FileList: TStringList;
  Output: string;
  AfterErrorRetry: integer; // Keeps track of retry attempts after error result
  UpdateRetry: integer; // Keeps track of retry attempts to get all files
begin
  AfterErrorRetry := 1;
  UpdateRetry := 1;

  // Invalidate our revision number cache
  FLocalRevision:=FRET_UNKNOWN_REVISION;
  FLocalRevisionWholeRepo:=FRET_UNKNOWN_REVISION;

  if (FDesiredRevision='') or (trim(FDesiredRevision)='HEAD') then
    Command := ' update --non-interactive ' + LocalRepository
  else
    Command := ' update --non-interactive -r ' + FDesiredRevision + ' ' + LocalRepository;

  FileList:=TStringList.Create;
  try
    // On Windows, at least certain SVN versions don't update everything.
    // So we try until there are no more files downloaded.
    FReturnCode:=ExecuteCommand(FRepoExecutable+command,Output,Verbose);

    FileList.Clear;
    ParseFileList(Output, FileList, []);

    // Detect when svn up cannot update any more files anymore.
    while (FileList.Count>0) and (UpdateRetry < MaxUpdateRetries) do
    begin
      // If command fails, e.g. due to misconfigured firewalls blocking ICMP etc, retry a few times
      while (ReturnCode <> 0) and (AfterErrorRetry < MaxErrorRetries) do
      begin
        if Pos('E155004',Output)>0 then
        {
        E155004: Working copy '<directory>' locked.
        run 'svn cleanup' to remove locks (type 'svn help cleanup' for details)
        }
        begin
          // Let's try to release locks.
          FReturnCode:=ExecuteCommand(FRepoExecutable+'cleanup --non-interactive '+ LocalRepository,Verbose); //attempt again
        end;
        Sleep(500); //Give everybody a chance to relax ;)
        FReturnCode:=ExecuteCommand(FRepoExecutable+command,Verbose); //attempt again
        AfterErrorRetry := AfterErrorRetry + 1;
      end;
      UpdateRetry := UpdateRetry + 1;
    end;
  finally
    FileList.Free;
  end;
end;

procedure TSVNClient.ParseFileList(const CommandOutput: string; var FileList: TStringList; const FilterCodes: array of string);
// Parses file lists from svn update and svn status outputs
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
      //Some sample files (using svn update and svn status):
      //A    fpctrunk\tests\webtbs\tw15683.pp
      //U    fpctrunk\compiler\ncal.pas
      //M       C:\Development\fpc\packages\bzip2\Makefile
      //I think I also saw something like:
      // u      C:\Development\fpc\packages\bzip2\Makefile
      //123456789
      // Also accept space in first column and entry on second column
      // Get the first character after a space in the first 2 columns:
      FileName:='';
      StatusCode:=Copy(Trim(Copy(AllFilesRaw[Counter],1,2)),1,1);
      SpaceAfterStatus:=PosEx(' ', AllFilesRaw[Counter], Pos(StatusCode, AllFilesRaw[Counter]));
      // Process if there are two spaces after the status character, and
      // we're either not filtering or we have a filter match
      if (Copy(AllFilesRaw[Counter], SpaceAfterStatus,2)='  ') and
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

procedure TSVNClient.LocalModifications(var FileList: TStringList);
var
  AllFiles: TStringList;
  Output: string='';
begin
  FReturnCode:=ExecuteCommand(FRepoExecutable+' status --depth infinity '+FLocalRepository,Output,Verbose);
  FileList.Clear;
  AllFiles:=TStringList.Create;
  try
    // Only return files that are (M)odified, (C)onflicting, mer(G)ed automatically
    ParseFileList(Output, AllFiles, ['C','G','M']);
    FileList.AddStrings(AllFiles);
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
  FReturnCode := ExecuteCommand(FRepoExecutable+' info ' + FLocalRepository,Output,Verbose);
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
        FLocalRevision:=FRET_UNKNOWN_REVISION;
        FLocalRevisionWholeRepo:=FRET_UNKNOWN_REVISION;
        FReturnCode:=FRET_LOCAL_REMOTE_URL_NOMATCH;
      end;
    end;
  end;
end;

procedure TSVNClient.GetLocalRevisions;
const
  BranchRevLength = Length('Last Changed Rev:');
  RevLength = Length('Revision:');
  RevExpression = '\:\s+(\d+)\s'; //regex to match revision in svn info
var
  LBranchRevision: string;
  LRevision: string; // Revision of repository as a whole
  Output: string;
  RevExtr: TRegExpr;
  RevCount: Integer;
begin
  // Only update if we have invalid revision info, in order to minimize svn info calls
  if (FLocalRevision=FRET_UNKNOWN_REVISION) or (FLocalRevisionWholeRepo=FRET_UNKNOWN_REVISION) then
  begin
    FReturnCode:=ExecuteCommand(FRepoExecutable+' info ' + FLocalRepository,Output,Verbose);
    // Could have used svnversion but that would have meant calling yet another command...
    // Get the part after "Revision:"...
    // unless we're in a branch/tag where we need "Last Changed Rev: "
    if FReturnCode=0 then
      begin
        // Use regex to try and extract from localized SVNs:
        // match exactly 2 occurences of the revision regex.
        RevCount:=0;
        RevExtr:=TRegExpr.Create;
        try
          RevExtr.Expression:=RevExpression;
          if RevExtr.Exec(Output) then begin
             Inc(RevCount);
             FLocalRevisionWholeRepo:=RevExtr.Match[1];
             if FLocalRevisionWholeRepo='' then FLocalRevisionWholeRepo:=FRET_UNKNOWN_REVISION;
             if RevExtr.ExecNext then begin
                Inc(RevCount); //we only have valid revision info when we get both repo and branch revision...
                FLocalRevision:=RevExtr.Match[1];
                if FLocalRevision='' then FLocalRevision:=FRET_UNKNOWN_REVISION;
             end;
          end;
        finally
          RevExtr.Free;
        end;
        if RevCount<>2 then
          begin
          // Regex failed; trying for English revision message (though this may be
          // superfluous with the regex)
          FLocalRevision:=FRET_UNKNOWN_REVISION;
          FLocalRevision:=trim(copy(Output,
            (pos('Last Changed Rev: ', Output) + BranchRevLength),
            6));
          FLocalRevisionWholeRepo:=FRET_UNKNOWN_REVISION;
          FLocalRevisionWholeRepo:=trim(copy(Output,
            (pos('Revision: ', Output) + RevLength),
            6));
          end;
      // If we happen to be in the root (no branch), cater for that:
      if FLocalRevision=FRET_UNKNOWN_REVISION then FLocalRevision:=FLocalRevisionWholeRepo;
      end
    else
      if Pos('E155036',LRevision)>0 then
        begin
        FLocalRevision:=FRET_UNKNOWN_REVISION;
        FLocalRevisionWholeRepo:=FRET_UNKNOWN_REVISION;
        FReturnCode:=FRET_WORKING_COPY_TOO_OLD;
        end;
  end;
end;

function TSVNClient.GetLocalRevision: string;
begin
  GetLocalRevisions;
  result:=FLocalRevision;
end;

function TSVNClient.GetLocalRevisionWholeRepo: string;
begin
  GetLocalRevisions;
  result:=FLocalRevisionWholeRepo;
end;


constructor Tsvnclient.Create;
begin
  inherited Create;
end;

destructor Tsvnclient.Destroy;
begin
  inherited Destroy;
end;
end.
