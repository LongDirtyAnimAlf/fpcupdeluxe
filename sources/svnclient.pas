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
  repoclient;

const
  // Custom return codes
  FRET_LOCAL_REMOTE_URL_NOMATCH = repoclient.FRET_LOCAL_REMOTE_URL_NOMATCH;
  FRET_WORKING_COPY_TOO_OLD = repoclient.FRET_WORKING_COPY_TOO_OLD;
  FRET_UNKNOWN_REVISION = repoclient.FRET_UNKNOWN_REVISION;

type
  TSVNClient = class(TRepoClient)
  private
    FUserName: string;
    FPassword: string;
  protected
    FLocalRevisionWholeRepo: string;
    procedure CheckOut(UseForce:boolean=false); override;
    procedure Update; override;
    function GetLocalRevision: string; override;
    function GetLocalRevisionWholeRepo: string;
    // Figure out branch and whole repo revisions for local repository
    procedure GetLocalRevisions;
    // Returns command snippet to set HTTP proxy config variables if needed
    function GetProxyCommand: string;
    function GetRepoExecutable: string; override;
    function GetRepoExecutableName: string; override;
    function FindRepoExecutable: string; override;
  public
    procedure CheckOutOrUpdate; override;
    function Commit(Message: string): boolean; override;
    function GetDiffAll: string; override;
    procedure LocalModifications(var FileList: TStringList); override;
    function LocalRepositoryExists: boolean; override;
    //Revision number of local repository - the repository wide revision number regardless of what branch we are in
    property LocalRevisionWholeRepo: string read GetLocalRevisionWholeRepo;
    procedure ParseFileList(const CommandOutput: string; var FileList: TStringList; const FilterCodes: array of string); override;
    procedure Revert; override;
    function CheckURL: boolean;
    // Run SVN log command for repository and put results into Log
    procedure Log(var Log: TStringList); override;
  published
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
  end;

  ESVNClientError = class(ERepoClientError);

implementation

uses
  {$IFDEF UNIX}
  BaseUnix,Unix,
  {$ENDIF}
  StrUtils, regexpr,
  InstallerCore,
  processutils,
  fpcuputil;

{ TSVNClient }
function TSVNClient.GetRepoExecutableName: string;
begin
  // Application name:
  result := 'svn';
end;

function TSVNClient.FindRepoExecutable: string;
var
  //Output: string;
  rv:integer;
begin
  Result := FRepoExecutable;

  while True do
  begin
    {$IFDEF DARWIN}
    if not FileExists(FRepoExecutable)
       then FRepoExecutable := '/opt/local/bin/svn'
       else break;
    if not FileExists(FRepoExecutable)
       then FRepoExecutable := '/opt/homebrew/bin/svn'
       else break;
    if not FileExists(FRepoExecutable)
       then FRepoExecutable := '/Library/Developer/CommandLineTools/usr/bin/svn'
       else break;
    {$ENDIF}

    {$IFDEF MSWINDOWS}
    // Some popular locations for SlikSVN, Subversion, and TortoiseSVN:
    // Covers both 32 bit and 64 bit Windows.
    if not FileExists(FRepoExecutable)
       then FRepoExecutable := GetEnvironmentVariable('ProgramFiles')+'\Subversion\bin\' + RepoExecutableName + '.exe'
       else break;
    if not FileExists(FRepoExecutable)
       then FRepoExecutable := GetEnvironmentVariable('ProgramFiles(x86)')+'\Subversion\bin\' + RepoExecutableName + '.exe'
       else break;
    if not FileExists(FRepoExecutable)
       then FRepoExecutable := GetEnvironmentVariable('ProgramFiles')+'\SlikSvn\bin\' + RepoExecutableName + '.exe'
       else break;
    if not FileExists(FRepoExecutable)
       then FRepoExecutable := GetEnvironmentVariable('ProgramFiles(x86)')+'\SlikSvn\bin\' + RepoExecutableName + '.exe'
       else break;
    if not FileExists(FRepoExecutable)
       then FRepoExecutable := GetEnvironmentVariable('ProgramFiles')+'\TorToiseSVN\bin\' + RepoExecutableName + '.exe'
       else break;
    if not FileExists(FRepoExecutable)
       then FRepoExecutable := GetEnvironmentVariable('ProgramFiles(x86)')+'\TorToiseSVN\bin\' + RepoExecutableName + '.exe'
       else break;
    //Directory where current executable is:
    if not FileExists(FRepoExecutable)
       then FRepoExecutable := (SafeGetApplicationPath + RepoExecutableName + '.exe')
       else break;
    {$ENDIF MSWINDOWS}

    // Look in path
    // Windows: will also look for <SVNName>.exe
    if not FileExists(FRepoExecutable) then
      FRepoExecutable := Which(RepoExecutableName);

    break;
  end;

  if (not FileExists(FRepoExecutable)) then
  begin
    //current directory. Note: potential for misuse by malicious program.
    {$ifdef mswindows}
    if FileExists(RepoExecutableName + '.exe') then
      FRepoExecutable := RepoExecutableName + '.exe';
    {$else}
    if FileExists(RepoExecutableName) then
      FRepoExecutable := RepoExecutableName;
    {$endif mswindows}
  end;

  // If file exists, check for valid svn executable
  if FileExists(FRepoExecutable) then
  begin
    //rv:=TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + ' --version', Verbose);
    //if rv<>0 then
    if (NOT CheckExecutable(FRepoExecutable, ['--version'], '', true)) then
    begin
      FRepoExecutable := '';
      //ThreadLog('SVN client found, but error code during check: '+InttoStr(rv),etError);
      ThreadLog('SVN client found, but error code during check !',etError);
    end
    else
    begin
      if (CheckExecutable(FRepoExecutable, ['--version'], 'pc-msys', true)) then
      begin
        FRepoExecutable := '';
        ThreadLog('SVN client found in path, but its from MSYS and that does not work with fpcupdeluxe.',etWarning);
      end;
      if (CheckExecutable(FRepoExecutable, ['--version'], 'pc-cygwin', true)) then
      begin
        FRepoExecutable := '';
        ThreadLog('SVN client found in path, but its from CYGWIN and that does not work with fpcupdeluxe.',etWarning);
      end;

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

function TSVNClient.GetRepoExecutable: string;
begin
  if not FileExists(FRepoExecutable) then
    FindRepoExecutable;
  if not FileExists(FRepoExecutable) then
    Result := ''
  else
    Result := FRepoExecutable;
end;

procedure TSVNClient.CheckOut(UseForce:boolean=false);
var
  i:integer;
  Command: string;
  Output: string = '';
  ProxyCommand: string;
  RetryAttempt: integer;
  ExecuteSpecialDue2EmptyString:boolean;
  TempOutputFile:string;
  TempOutputSL:TStringList;
begin
  // Invalidate our revision number cache
  FLocalRevision := FRET_UNKNOWN_REVISION;
  FLocalRevisionWholeRepo := FRET_UNKNOWN_REVISION;

  ProxyCommand:=GetProxyCommand;

  // flag used to signal the need of a special command due to some TProcess specialities (see comments below)
  ExecuteSpecialDue2EmptyString:=False;

  // Avoid
  // svn: E175002: OPTIONS of 'https://lazarus-ccr.svn.sourceforge.net/svnroot/lazarus-ccr/components/fpspreadsheet': Server certificate verification failed: issuer is not trusted (https://lazarus-ccr.svn.sourceforge.net)
  // by --trust-server-cert

  Command := '';

  if Length(UserName)>0 then
  begin
    // svn quirk : even if no password is needed, it needs an empty password.
    // to prevent deleting this empty string, we fill it here with a special placeholder: emptystring, that gets replaced later, inside ExecuteCommand
    if Length(Password)=0 then Password:='emptystring';
    Command:=' --username '+UserName+' --password '+Password;
  end;

  if ExportOnly
     then Command:=' export --quiet --force '+ProxyCommand+Command+' --non-interactive --trust-server-cert -r '
     else
     begin
       Command:=' checkout --quiet '+ProxyCommand+Command+' --non-interactive --trust-server-cert -r ';
       if UseForce then Command:=StringReplace(Command,' checkout ',' checkout --force ',[]);
     end;

  if (DesiredRevision = '') or (Uppercase(trim(DesiredRevision)) = 'HEAD') then
    Command:=Command+'HEAD '+Repository+' '+DoubleQuoteIfNeeded(LocalRepository)
  else
    Command:=Command+DesiredRevision+' '+Repository+' '+DoubleQuoteIfNeeded(LocalRepository);

  {$IFNDEF MSWINDOWS}
  // due to the fact that strnew returns nil for an empty string, we have to use something special to process a command with empty strings on non windows systems
  // see this [Function StringsToPCharList(List : TStrings) : PPChar] inside process.inc for Unix
  if Pos('emptystring',Command)>0 then
  begin
    Command:=StringReplace(Command,'emptystring','""',[rfReplaceAll,rfIgnoreCase]);
    TempOutputFile:=ChangeFileExt(GetTempFileName(GetTempDir(false),'FPCUPTMP'),'svn');
    Command:=Command+' &> '+TempOutputFile;
    ExecuteSpecialDue2EmptyString:=True;
  end;
  {$ENDIF}

  RetryAttempt := 0;

  while true do
  begin
    {$IFDEF UNIX}
    if ExecuteSpecialDue2EmptyString then
    begin
      //FReturnCode := ExecuteProcess(FRepoExecutable,Command,[]);
      //FReturnCode := FpExecL(FRepoExecutable,[Command]);
      //FReturnCode := FpExecL('/bin/sh',['-c',Command]);
      FReturnCode := fpSystem(FRepoExecutable+' '+Command);
      sleep(5000);
      //if FReturnCode=-1 then FReturnCode:=fpgeterrno;
      if FileExists(TempOutputFile) then
      begin
        TempOutputSL:=TStringList.Create();
        try
          TempOutputSL.LoadFromFile(TempOutputFile);
          Output:=TempOutputSL.ToString;
        finally
          TempOutputSL.Free();
        end;
        SysUtils.DeleteFile(TempOutputFile);
      end;
    end
    else
    {$ENDIF}
    FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + Command, Output, Verbose);
    FReturnOutput := Output;

    if (ReturnCode=AbortedExitCode) then break;

    if (ReturnCode=0) then break else
    begin
      ThreadLog('SVN client error return code: '+InttoStr(ReturnCode),etWarning);

      Inc(RetryAttempt);

      //Give everybody a chance to relax ;)
      Sleep(1000);

      //E175002: Connection failure
      //E730065: Host unreacheable.
      //E170013: Unable to connect to a repository at URL
      //E731001: Host is unknown.
      //E175012: Connection timed out
      //E120108: The server unexpectedly closed the connection
      if ((Pos('E175002', Output)>0) OR (Pos('E730065', Output)>0) OR (Pos('E170013', Output)>0) OR (Pos('E731001', Output)>0) OR (Pos('E175012', Output)>0) OR (Pos('E120108', Output)>0)) then
      begin
        //do a simple retry in case of connection failures
        if RetryAttempt>CONNECTIONMAXRETRIES then break else
        begin
          // remove locks if any
          FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + ' cleanup --non-interactive ' + DoubleQuoteIfNeeded(LocalRepository), Verbose);
          if (ReturnCode=AbortedExitCode) then exit;
          // try again
          continue;
        end;
      end;

      if RetryAttempt>ERRORMAXRETRIES then break;

      //E155004: Working copy '<directory>' locked.
      //run 'svn cleanup' first to remove eventual locks (type 'svn help cleanup' for details)
      if (Pos('E155004', Output) > 0) OR (Pos('E175002', Output) > 0) then
      begin
        // Let's try one time to fix it and don't update FReturnCode here
        FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + ' cleanup --non-interactive ' + DoubleQuoteIfNeeded(LocalRepository), Verbose); //attempt again
        if (ReturnCode=AbortedExitCode) then exit;
        // We probably ended up with a local repository where not all files were checked out
        // Let's call update to finalize.
        Update;
      end;

      // svn: E155036: Please see the 'svn upgrade' command
      // svn: E155036: The working copy is too old to work with client. You need to upgrade the working copy first
      if Pos('E155036', Output) > 0 then
      begin
        // Let's try one time upgrade to fix it (don't update FReturnCode here)
        FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + ' upgrade '+ProxyCommand+' --non-interactive ' + DoubleQuoteIfNeeded(LocalRepository), Verbose); //attempt again
        if (ReturnCode=AbortedExitCode) then exit;
        // Now update again:
        Update;
      end;
    end;
  end;

end;

procedure TSVNClient.Update;
var
  i:integer;
  Command: string;
  FileList: TStringList;
  Output: string = '';
  ProxyCommand: string;
  AfterErrorRetry: integer; // Keeps track of retry attempts after error result
  UpdateRetry: integer;     // Keeps track of retry attempts to get all files
  ExecuteSpecialDue2EmptyString:boolean;
  TempOutputFile:string;
  TempOutputSL:TStringList;
begin

  if ExportOnly then
  begin
    FReturnCode := 0;
    exit;
  end;

  AfterErrorRetry := 1;
  UpdateRetry := 1;
  ProxyCommand:=GetProxyCommand;

  ExecuteSpecialDue2EmptyString:=false;

  // Invalidate our revision number cache
  FLocalRevision := FRET_UNKNOWN_REVISION;
  FLocalRevisionWholeRepo := FRET_UNKNOWN_REVISION;

  Command := '';

  if Length(UserName)>0 then
  begin
    // svn quirk : even if no password is needed, it needs an empty password.
    // to prevent deleting this empty string, we fill it here with a special placeholder: emptystring, that gets replaced later, inside ExecuteCommand
    if Length(Password)=0 then Password:='emptystring';
    Command := ' --username ' + UserName + ' --password ' + Password;
  end;

  if (DesiredRevision = '') or (Uppercase(trim(DesiredRevision)) = 'HEAD') then
    Command := ' update ' + ProxyCommand + Command + ' --quiet --non-interactive --trust-server-cert -r HEAD ' + DoubleQuoteIfNeeded(LocalRepository)
  else
    Command := ' update ' + ProxyCommand + Command + ' --quiet --non-interactive --trust-server-cert -r ' + DesiredRevision + ' ' + DoubleQuoteIfNeeded(LocalRepository);

  {$IFNDEF MSWINDOWS}
  // due to the fact that strnew returns nil for an empty string, we have to use something special to process a command with empty strings on non windows systems
  // see this [Function StringsToPCharList(List : TStrings) : PPChar] inside process.inc for Unix
  if Pos('emptystring',Command)>0 then
  begin
    Command:=StringReplace(Command,'emptystring','""',[rfReplaceAll,rfIgnoreCase]);
    TempOutputFile:=ChangeFileExt(GetTempFileName(GetTempDir(false),'FPCUPTMP'),'svn');
    Command:=Command + ' &> '+TempOutputFile;
    ExecuteSpecialDue2EmptyString:=True;
  end;
  {$ENDIF}

  // always perform a cleaup before doing anything else ... just to be sure !
  FReturnCode:=TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + ' cleanup --non-interactive ' + DoubleQuoteIfNeeded(LocalRepository), Verbose);

  if (ReturnCode=AbortedExitCode) then exit;

  FileList := TStringList.Create;
  try
    // On Windows, at least certain SVN versions don't update everything.
    // So we try until there are no more files downloaded.

    {$IFDEF UNIX}
    if ExecuteSpecialDue2EmptyString then
    begin
      //FReturnCode := ExecuteProcess(FRepoExecutable,Command,[]);
      //FReturnCode := FpExecL(FRepoExecutable,[Command]);
      //FReturnCode := FpExecL('/bin/sh',['-c',Command]);
      FReturnCode := fpSystem(FRepoExecutable+' '+Command);
      sleep(2500);
      //if FReturnCode=-1 then FReturnCode:=fpgeterrno;
      if FileExists(TempOutputFile) then
      begin
        TempOutputSL:=TStringList.Create();
        try
          TempOutputSL.LoadFromFile(TempOutputFile);
          Output:=TempOutputSL.ToString;
        finally
          TempOutputSL.Free();
        end;
        SysUtils.DeleteFile(TempOutputFile);
      end;
    end
    else
    {$ENDIF}
    FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + command, Output, Verbose);
    FReturnOutput := Output;

    if (ReturnCode=AbortedExitCode) then exit;

    if (ReturnCode <> 0) then
    begin
      ThreadLog('SVN client error return code: '+InttoStr(ReturnCode),etError);
    end;

    if (Pos('An obstructing working copy was found', Output) > 0) then
    begin
      ThreadLog('SVN reported than an obstructing working copy was found.',etError);
      ThreadLog('Please try to resolve.',etError);
      FReturnCode := -1;
      exit;
    end;

    FileList.Clear;
    ParseFileList(Output, FileList, []);

    // Detect when svn up cannot update any more files anymore.
    while (FileList.Count > 0) and (UpdateRetry < CONNECTIONMAXRETRIES) do
    begin
      // If command fails, e.g. due to misconfigured firewalls blocking ICMP etc, retry a few times
      while (ReturnCode <> 0) and (AfterErrorRetry < ERRORMAXRETRIES) do
      begin
        if Pos('E155004', Output) > 0 then
        {
        E155004: Working copy '<directory>' locked.
        run 'svn cleanup' to remove locks (type 'svn help cleanup' for details)
        }
        begin
          // Let's try to release locks; don't update FReturnCode
          FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + ' cleanup --non-interactive ' + DoubleQuoteIfNeeded(LocalRepository), Verbose); //attempt again
          if (ReturnCode=AbortedExitCode) then exit;
        end;
        //Give everybody a chance to relax ;)
        Sleep(500);
        // attempt again !!

        {$IFDEF UNIX}
        if ExecuteSpecialDue2EmptyString then
        begin
          //FReturnCode := ExecuteProcess(FRepoExecutable,Command,[]);
          //FReturnCode := FpExecL(FRepoExecutable,[Command]);
          //FReturnCode := FpExecL('/bin/sh',['-c',Command]);
          FReturnCode := fpSystem(FRepoExecutable+' '+Command);
          sleep(5000);
          //if FReturnCode=-1 then FReturnCode:=fpgeterrno;
          if FileExists(TempOutputFile) then
          begin
            TempOutputSL:=TStringList.Create();
            try
              TempOutputSL.LoadFromFile(TempOutputFile);
              Output:=TempOutputSL.ToString;
            finally
              TempOutputSL.Free();
            end;
            SysUtils.DeleteFile(TempOutputFile);
          end;
        end
        else
        {$ENDIF}

        // get problem files, and do something about it !
        {
        FileList.Clear;
        ParseFileList(Output, FileList, ['?','!']);
        }
        FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + command, FReturnOutput, Verbose);
        if (ReturnCode=AbortedExitCode) then exit;

        AfterErrorRetry := AfterErrorRetry + 1;

        // last resort measures
        if (AfterErrorRetry = ERRORMAXRETRIES) then
        begin
          //revert local changes to try to cleanup errors ...
          //FReturnCode := TInstaller(Parent).ExecuteCommandCompat(DoubleQuoteIfNeeded(FRepoExecutable) + ' revert -R '+ProxyCommand+' --non-interactive ' + DoubleQuoteIfNeeded(LocalRepository), Verbose); //revert changes
          FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + ' cleanup --non-interactive --remove-unversioned --remove-ignored ' + DoubleQuoteIfNeeded(LocalRepository), Verbose); //attempt again
          if (ReturnCode=AbortedExitCode) then exit;

        end;

      end;
      UpdateRetry := UpdateRetry + 1;
    end;
  finally
    FileList.Free;
  end;
end;

procedure TSVNClient.CheckOutOrUpdate;
begin
  if LocalRepositoryExists = false then
  begin
    if FReturnCode = FRET_LOCAL_REMOTE_URL_NOMATCH then
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
      if (FReturnCode<>AbortedExitCode) then Update;
    end;
  end
  else
  begin
    // Update
    Update;
  end;
end;

function TSVNClient.Commit(Message: string): boolean;
begin
  inherited Commit(Message);
  if ExportOnly then
  begin
    Result:=True;
    exit;
  end;
  FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' commit '+GetProxyCommand+' --message='+Message, LocalRepository, FReturnOutput, Verbose);
  Result:=(FReturnCode=0);
end;

function TSVNClient.GetDiffAll: string;
var
  aFile:string;
  aResult:TStringList;
  aProcess:TExternalTool;
begin
  result := '';
  FReturnCode := 0;

  if ExportOnly then
  begin
    exit;
  end;

  aProcess:=nil;

  {$ifdef  MSWINDOWS}
  aProcess := TExternalTool.Create(nil);
  aProcess.Process.Executable := GetEnvironmentVariable('COMSPEC');
  if NOT FileExists(aProcess.Process.Executable) then aProcess.Process.Executable := 'c:\windows\system32\cmd.exe';
  aProcess.Process.Parameters.Add('/c');
  {$endif  MSWINDOWS}

  {$ifdef LINUX}
  aProcess := TExternalTool.Create(nil);
  aProcess.Process.Executable := '/bin/sh';
  aProcess.Process.Parameters.Add('-c');
  {$endif LINUX}

  if Assigned(aProcess) then
  begin
    if NOT FileExists(aProcess.Process.Executable) then
    begin
      aProcess.Free;
      aProcess:=nil;
    end;
  end;

  if Assigned(aProcess) then
  begin
    aFile := ChangeFileExt(GetTempFileName(GetTempDir(false),'FPCUPTMP'),'diff');
    aProcess.Process.CurrentDirectory:=LocalRepository;
    aProcess.Process.Parameters.Add(DoubleQuoteIfNeeded(FRepoExecutable) + GetProxyCommand + ' diff -x --ignore-space-change'+' . > ' + aFile);
    try
      aProcess.ExecuteAndWait;
      FReturnCode:=aProcess.ExitCode;
      if (FReturnCode=0) AND (FileExists(aFile)) then
      begin
        aResult:=TStringList.Create;
        try
          aResult.LoadFromFile(aFile);
          result:=aResult.Text;
        finally
          aResult.Free;
        end;
      end;
    finally
      aProcess.Free;
      aProcess:=nil;
      DeleteFile(aFile);
    end;
  end;

  if result='' then
  begin
    // Using proxy more for completeness here
    //FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + GetProxyCommand + ' diff '+' .', LocalRepository, Result, Verbose);
    // with external diff program
    //FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + GetProxyCommand + ' diff --diff-cmd diff --extensions "--binary -wbua"'+' .', LocalRepository, Result, Verbose);
    // ignoring whitespaces
    FReturnCode := TInstaller(Parent).ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + GetProxyCommand + ' diff -x --ignore-space-change'+' .', LocalRepository, result, Verbose);
  end;

  FReturnOutput := result;
end;

procedure TSVNClient.Log(var Log: TStringList);
var
  s: string = '';
begin
  // Using proxy more for completeness here
  FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + ' log ' + GetProxyCommand + ' ' + DoubleQuoteIfNeeded(LocalRepository), s, Verbose);
  FReturnOutput := s;
  Log.Text := s;
end;

procedure TSVNClient.Revert;
begin
  if ExportOnly then
  begin
    FReturnCode := 0;
    exit;
  end;
  FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + ' revert '+GetProxyCommand+' --recursive ' + DoubleQuoteIfNeeded(LocalRepository), FReturnOutput, Verbose);
end;

function TSVNClient.CheckURL: boolean;
var
  Output:string;
  aFile,aURL:string;
  i:integer;
begin
  aURL:=Repository;

  Output:=ExcludeTrailingSlash(Repository);
  aFile:=FileNameFromURL(Output);
  i:=Pos(aFile,Output);
  if (i>0) then aURL:=Copy(Output,1,(i-1));

  Output:='';
  FReturnCode := TInstaller(Parent).ExecuteCommand(FRepoExecutable,['ls',aURL], Output, False);
  //FReturnCode := TInstaller(Parent).ExecuteCommand(FRepoExecutable,['ls',Repository], Output, False);
  //FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + ' ls '+ Repository, Output, False);
  //FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + ' ls --depth empty '+ Repository, Output, False);
  result:=((FReturnCode=0) AND (Pos(aFile,Output)>0));
  //result:=(Output=GetFileNameFromURL(Repository));
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
  AllFilesRaw := TStringList.Create;
  try
    AllFilesRaw.Text := CommandOutput;
    for Counter := 0 to Pred(AllFilesRaw.Count) do
    begin
      //Some sample files (using svn update and svn status):

      //‘A’ Item is scheduled for Addition.
      //‘U’ Item is scheduled for Update.
      //‘D’ Item is scheduled for Deletion.
      //‘E’ Item already Existed.
      //‘E’ Item already Existed.
      //‘G’ Item is Merged.
      //‘M’ Item has been modified.
      //‘R’ Item has been replaced in your working copy. This means the file was scheduled for deletion, and then a new file with the same name was scheduled for addition in its place.
      //‘C’ The contents (as opposed to the properties) of the item conflict with updates received from the repository.
      //‘X’ Item is related to an externals definition.
      //‘I’ Item is being ignored (e.g. with the svn:ignore property).
      //’?’ Item is not under version control.
      //’!’ Item is missing (e.g. you moved or deleted it without using svn). This also indicates that a directory is incomplete (a checkout or update was interrupted).
      //’~’ Item is versioned as one kind of object (file, directory, link), but has been replaced by different kind of object.

      // Also accept space in first column and entry on second column
      // Get the first character after a space in the first 2 columns:
      FileName := '';
      StatusCode := Copy(Trim(Copy(AllFilesRaw[Counter], 1, 2)), 1, 1);
      SpaceAfterStatus := PosEx(' ', AllFilesRaw[Counter], Pos(StatusCode, AllFilesRaw[Counter]));
      // Process if there are two spaces after the status character, and
      // we're either not filtering or we have a filter match
      if (Copy(AllFilesRaw[Counter], SpaceAfterStatus, 2) = '  ') and ((High(FilterCodes) = 0) or
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

procedure TSVNClient.LocalModifications(var FileList: TStringList);
var
  AllFiles: TStringList;
  Output: string = '';
begin
  if ExportOnly then
  begin
    FReturnCode := 0;
    exit;
  end;

  FReturnCode := TInstaller(Parent).ExecuteCommand(FRepoExecutable,['status','--depth','infinity',LocalRepository], Output, Verbose);

  FReturnOutput := Output;
  FileList.Clear;
  AllFiles := TStringList.Create;
  try
    // Only return files that are (M)odified, (C)onflicting, mer(G)ed automatically
    ParseFileList(Output, AllFiles, ['C', 'G', 'M']);
    FileList.AddStrings(AllFiles);
  finally
    AllFiles.Free;
  end;
end;

function TSVNClient.LocalRepositoryExists: boolean;
const
  URLExpression='https?:\/\/(www\.)?[-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b([-a-zA-Z0-9()@:%_\+.~#?&//=]*)';
  SVNExpression='svn:\/\/svn\.[-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b([-a-zA-Z0-9()@:%_\+.~#?&//=]*)';
var
  Output: string = '';
  URL: string;
  URLExtr: TRegExpr;
begin
  Result := false;
  FReturnCode := 0;
  if ExportOnly then exit;
  if NOT ValidClient then exit;
  if NOT DirectoryExists(LocalRepository) then exit;

  FReturnCode := TInstaller(Parent).ExecuteCommand(FRepoExecutable,['info',LocalRepository],Output,Verbose);
  FReturnOutput := Output;

  // If command fails due to wrong version, try again
  if (ReturnCode <> 0) then
  begin
    // svn: E155036: Please see the 'svn upgrade' command
    // svn: E155036: The working copy is too old to work with client. You need to upgrade the working copy first
    // Let's try one time upgrade to fix it (don't update FReturnCode here)
    if Pos('E155036', Output) > 0 then TInstaller(Parent).ExecuteCommand(FRepoExecutable,['upgrade','--non-interactive',LocalRepository],Verbose);
    //Give everybody a chance to relax ;)
    Sleep(500);
    //attempt again
    FReturnCode := TInstaller(Parent).ExecuteCommand(FRepoExecutable,['info',LocalRepository],Output,Verbose);
    FReturnOutput := Output;
  end;

  // This is already covered by setting stuff to false first
  //if Pos('is not a working copy', Output.Text) > 0 then result:=false;
  if (Pos('Path', Output) > 0) then
  begin
    // There is an SVN repository here.
    // Output from info command can include:
    // URL: http://svn.freepascal.org/svn/fpc/branches/fixes_3_0
    // Repository URL might differ from the one we've set though
    // Parse the URL

    URL:='';

    URLExtr := TRegExpr.Create;
    try
      URLExtr.Expression := URLExpression;
      if URLExtr.Exec(Output) then
      begin
        URL := URLExtr.Match[0];
      end;
      if (URL='') then
      begin
        URLExtr.Expression := SVNExpression;
        if URLExtr.Exec(Output) then
        begin
          URL := URLExtr.Match[0];
        end;
      end;
    finally
      URLExtr.Free;
    end;

    URL:=IncludeTrailingSlash(URL);

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
        FLocalRevisionWholeRepo := FRET_UNKNOWN_REVISION;
        FReturnCode             := FRET_LOCAL_REMOTE_URL_NOMATCH;
        Repository              := URL;
      end;
    end;
  end;
end;

procedure TSVNClient.GetLocalRevisions;
const
  BranchRevTarget = 'Last Changed Rev:';
  BranchRevLength = Length(BranchRevTarget);
  RevTarget = 'Revision:';
  RevLength = Length(RevTarget);
  RevExpression = '\:\s+(\d+)\s'; //regex to match revision in svn info
var
  i:integer;
  LRevision: string = ''; // Revision of repository as a whole
  Output: string = '';
  RevExtr: TRegExpr;
  RevCount: integer;

  Command: string;
  ExecuteSpecialDue2EmptyString:boolean;
  TempOutputFile:string;
  TempOutputSL:TStringList;
begin

  // Only update if we have invalid revision info, in order to minimize svn info calls
  if (FLocalRevision = FRET_UNKNOWN_REVISION) or (FLocalRevisionWholeRepo = FRET_UNKNOWN_REVISION) then
  begin
    if ExportOnly then
       begin
         if (DesiredRevision = '') or (trim(DesiredRevision) = 'HEAD') then
            begin

              Command := '';

              if Length(UserName)>0 then
              begin
                // svn quirk : even if no password is needed, it needs an empty password.
                // to prevent deleting this empty string, we fill it here with a special placeholder: emptystring, that gets replaced later, inside ExecuteCommand
                if Length(Password)=0 then Password:='emptystring';
                Command:=' --username '+UserName+' --password '+Password;
              end;

              Command:=' info '+GetProxyCommand + Command + ' --non-interactive --trust-server-cert ' + Repository;

              {$IFNDEF MSWINDOWS}
              // due to the fact that strnew returns nil for an empty string, we have to use something special to process a command with empty strings on non windows systems
              // see this [Function StringsToPCharList(List : TStrings) : PPChar] inside process.inc for Unix
              if Pos('emptystring',Command)>0 then
              begin
                Command:=StringReplace(Command,'emptystring','""',[rfReplaceAll,rfIgnoreCase]);
                TempOutputFile:=ChangeFileExt(GetTempFileName(GetTempDir(false),'FPCUPTMP'),'svn');
                Command:=Command+' &> '+TempOutputFile;
                ExecuteSpecialDue2EmptyString:=True;
              end;
              {$ENDIF}

              {$IFDEF UNIX}
              if ExecuteSpecialDue2EmptyString then
              begin
                //FReturnCode := ExecuteProcess(FRepoExecutable,Command,[]);
                //FReturnCode := FpExecL(FRepoExecutable,[Command]);
                //FReturnCode := FpExecL('/bin/sh',['-c',Command]);
                FReturnCode := fpSystem(FRepoExecutable+' '+Command);
                sleep(1000);
                //if FReturnCode=-1 then FReturnCode:=fpgeterrno;
                if FileExists(TempOutputFile) then
                begin
                  TempOutputSL:=TStringList.Create;
                  try
                    TempOutputSL.LoadFromFile(TempOutputFile);
                    Output:=TempOutputSL.ToString;
                  finally
                    TempOutputSL.Free;
                  end;
                  SysUtils.DeleteFile(TempOutputFile);
                end;
              end
              else
              {$ENDIF}
              FReturnCode := TInstaller(Parent).ExecuteCommand(DoubleQuoteIfNeeded(FRepoExecutable) + Command, Output, False);
            end
            else
            begin
              FLocalRevision := DesiredRevision;
              FLocalRevisionWholeRepo := DesiredRevision;
              FReturnCode := 0;
              exit;
            end;
       end
       else FReturnCode := TInstaller(Parent).ExecuteCommand(FRepoExecutable,['info',LocalRepository],Output,False);


    FReturnOutput := Output;
    // Could have used svnversion but that would have meant calling yet another command...
    // Get the part after "Revision:"...
    // unless we're in a branch/tag where we need "Last Changed Rev: "
    if (FReturnCode=0) then
    begin
      // Use regex to try and extract from localized SVNs:
      // match exactly 2 occurences of the revision regex.
      RevCount := 0;
      if (Length(Output)>0) then
      begin
        RevExtr := TRegExpr.Create;
        try
          RevExtr.Expression := RevExpression;
          if RevExtr.Exec(Output) then
          begin
            Inc(RevCount);
            FLocalRevisionWholeRepo := RevExtr.Match[1];
            if FLocalRevisionWholeRepo = '' then
              FLocalRevisionWholeRepo := FRET_UNKNOWN_REVISION;
            if RevExtr.ExecNext then
            begin
              Inc(RevCount); //we only have valid revision info when we get both repo and branch revision...
              FLocalRevision := RevExtr.Match[1];
              if FLocalRevision = '' then
                FLocalRevision := FRET_UNKNOWN_REVISION;
            end;
          end;
        finally
          RevExtr.Free;
        end;
      end;
      if RevCount=0 then
      begin
        FLocalRevision := FRET_UNKNOWN_REVISION;
        FLocalRevisionWholeRepo := FRET_UNKNOWN_REVISION;
      end
      else
      if RevCount <> 2 then
      begin
        // Regex failed; trying for English revision message (though this may be
        // superfluous with the regex)
        FLocalRevision := FRET_UNKNOWN_REVISION;
        FLocalRevision := trim(copy(Output, (pos(BranchRevTarget, Output) + BranchRevLength), 6));
        FLocalRevisionWholeRepo := FRET_UNKNOWN_REVISION;
        FLocalRevisionWholeRepo := trim(copy(Output, (pos(RevTarget, Output) + RevLength), 6));
      end;
      // If we happen to be in the root (no branch), cater for that:
      if FLocalRevision = FRET_UNKNOWN_REVISION then
        FLocalRevision := FLocalRevisionWholeRepo;
    end
    else
    // Info call gave error, so we don't know the local revisions now
    begin
      FLocalRevision := FRET_UNKNOWN_REVISION;
      FLocalRevisionWholeRepo := FRET_UNKNOWN_REVISION;
      if Pos('E155036', LRevision) > 0 then
        FReturnCode := FRET_WORKING_COPY_TOO_OLD
      else
        FReturnCode := FRET_NONEXISTING_REPO;
    end;
  end;
end;

function TSVNClient.GetProxyCommand: string;
begin
  if HTTPProxyHost<>'' then
  begin
    result:='--config-option servers:global:http-proxy-host='+HTTPProxyHost;
    if HTTPProxyPort<>0 then
      result:=result+' --config-option servers:global:http-proxy-port='+IntToStr(HTTPProxyPort);
    if HTTPProxyUser<>'' then
      result:=result+' --config-option servers:global:http-proxy-username='+HTTPProxyUser;
    if HTTPProxyPassword<>'' then
      result:=result+' --config-option servers:global:http-proxy-password='+HTTPProxyPassword;
    //result:=result+' ';
  end
  else
  begin
    result:='';
  end;
end;

function TSVNClient.GetLocalRevision: string;
begin
  GetLocalRevisions;
  Result := FLocalRevision;
end;

function TSVNClient.GetLocalRevisionWholeRepo: string;
begin
  GetLocalRevisions;
  Result := FLocalRevisionWholeRepo;
end;

end.
