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
  FRET_UNKNOWN_REVISION=-4;

type
  ESVNClientError = class(Exception);
  { TSVNClient }

  TSVNClient = class(TObject)
  private
    FLocalRepository: string;
    FLocalRevision: integer;
    FLocalRevisionWholeRepo: integer;
    FRepositoryURL: string;
    FReturnCode: integer;
    FDesiredRevision: string;
    FSVNExecutable: string;
    FVerbose: boolean;
    function GetLocalRevision: integer;
    function GetLocalRevisionWholeRepo: integer;
    procedure GetLocalRevisions;
    function GetSVNExecutable: string;
    // Makes sure non-empty strings have a / at the end.
    function IncludeTrailingSlash(AValue: string): string;
    procedure SetDesiredRevision(AValue: string);
    procedure SetLocalRepository(AValue: string);
    procedure SetRepositoryURL(AValue: string);
    procedure SetSVNExecutable(AValue: string);
    procedure SetVerbose(AValue: boolean);
  public
    //Performs an SVN checkout (initial download), unless otherwise specified HEAD (latest revision) only for speed
    //Note: it's often easier to call CheckOutOrUpdate; that also has some more network error recovery built in
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
    //Note: it's often easier to call CheckOutOrUpdate; that also has some more network error recovery built in
    procedure Update;
    //Get/set desired revision to checkout/pull to (if none given, use HEAD)
    property DesiredRevision: string read FDesiredRevision write SetDesiredRevision;
    //Shows list of files that have been modified locally (and not committed)
    procedure LocalModifications(var FileList: TStringList);
    //Checks to see if local directory is a valid SVN repository for the repository URL given (if any)
    function LocalRepositoryExists: boolean;
    //Local directory that has an SVN repository/checkout.
    //When setting, relative paths will be expanded; trailing path delimiters will be removed
    property LocalRepository: string read FLocalRepository write SetLocalRepository;
    //Revision number of local repository: branch revision number if we're in a branch.
    property LocalRevision: integer read GetLocalRevision;
    //Revision number of local repository - the repository wide revision number regardless of what branch we are in
    property LocalRevisionWholeRepo: integer read GetLocalRevisionWholeRepo;
    //Parses output given by some commands (svn update, svn status) and returns files.
    // Files are marked by single characters (U,M,etc); you can filter on ore mor of these (pass [''] if not required).
    procedure ParseFileList(const CommandOutput: string; var FileList: TStringList; const FilterCodes: array of string);
    //URL where central (remote) SVN repository is placed
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

procedure TSVNClient.SetLocalRepository(AValue: string);
// Sets local repository, converting relative path to absolute path
// and adding a trailing / or \
begin
  if FLocalRepository=AValue then Exit;
  FLocalRepository:=ExcludeTrailingPathDelimiter(ExpandFileName(AValue));
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
  FReturnCode:=ExecuteCommand(SVNExecutable+Command,Output,Verbose);
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
        FReturnCode:=ExecuteCommand(SVNExecutable+' cleanup --non-interactive '+ LocalRepository,Verbose); //attempt again
        // We probably ended up with a local repository where not all files were checked out.
        // Let's call update to do so.
        Update;
      end;
      Sleep(500); //Give everybody a chance to relax ;)
      FReturnCode:=ExecuteCommand(SVNExecutable+Command,Output,Verbose); //attempt again
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
    FReturnCode:=ExecuteCommand(SVNExecutable+command,Output,Verbose);

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
          FReturnCode:=ExecuteCommand(SVNExecutable+'cleanup --non-interactive '+ LocalRepository,Verbose); //attempt again
        end;
        Sleep(500); //Give everybody a chance to relax ;)
        FReturnCode:=ExecuteCommand(SVNExecutable+command,Verbose); //attempt again
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
  FReturnCode:=ExecuteCommand(SVNExecutable+' status --depth infinity '+FLocalRepository,Output,Verbose);
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
  FReturnCode := ExecuteCommand(SVNExecutable+' info ' + FLocalRepository,Output,Verbose);
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
var
  LBranchRevision: string;
  LRevision: string; // Revision of repository as a whole
  Output: string;
begin
  // Only update if we have invalid revision info, in order to minimize svn info calls
  if (FLocalRevision<0) or (FLocalRevisionWholeRepo<0) then
  begin
    FReturnCode:=ExecuteCommand(SVNExecutable+' info ' + FLocalRepository,Output,Verbose);
    // Could have used svnversion but that would have meant calling yet another command...
    // Get the part after "Revision:"...
    // unless we're in a branch/tag where we need "Last Changed Rev: "
    if FReturnCode=0 then
      begin
      // This is going to be problematic for localized SVNs....
      FLocalRevision:=StrToIntDef(trim(copy(Output,
        (pos('Last Changed Rev: ', Output) + BranchRevLength),
        6)), FRET_UNKNOWN_REVISION);
      FLocalRevisionWholeRepo:=StrToIntDef(trim(copy(Output,
        (pos('Revision: ', Output) + RevLength),
        6)), FRET_UNKNOWN_REVISION);
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

function TSVNClient.GetLocalRevision: integer;
begin
  GetLocalRevisions;
  result:=FLocalRevision;
end;

function TSVNClient.GetLocalRevisionWholeRepo: integer;
begin
  GetLocalRevisions;
  result:=FLocalRevisionWholeRepo;
end;


constructor Tsvnclient.Create;
begin
  FLocalRepository := '';
  FRepositoryURL := '';
  FDesiredRevision:='';
  FLocalRevision:=FRET_UNKNOWN_REVISION;
  FLocalRevisionWholeRepo:=FRET_UNKNOWN_REVISION;
  FReturnCode := 0;
  FSVNExecutable := '';
  FindSvnExecutable; //Do this now so hopefully the SVNExecutable property is valid.
end;

destructor Tsvnclient.Destroy;
begin
  inherited Destroy;
end;
end.
