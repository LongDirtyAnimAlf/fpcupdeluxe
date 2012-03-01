unit installerCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SvnClient, processutils,m_crossinstaller;

type

  { TInstaller }

  TInstaller = class(TObject)
  private
    function GetMake: string;
  protected
    FBaseDirectory: string;
    FBinUtils:TStringList;
    FBunzip2:string;
    FCompiler: string;
    FCompilerOptions: string;
    FCrossCPU_Target: string;
    FCrossOS_Target: string;
    FDesiredRevision: string;
    FLogFile:Text;
    FLogVerboseFile:Text;
    FMake:string;
    FMakeDir:string;
    FSVNClient:TSVNClient;
    FSVNDirectory:string;
    FSVNUpdated:boolean;
    FURL: string;
    FVerbose: boolean;
    FTar:string;
    FUnzip:string;
    ProcessEx:TProcessEx;
    property Make:string read GetMake;
    function CheckAndGetNeededExecutables: boolean;
    function CheckExecutable(Executable, Parameters, ExpectOutput: string): boolean;
    procedure CreateBinutilsList;
    procedure CreateStoreSVNDiff(DiffFileName: string;UpdateWarnings: TStringList);
    function DownloadBinUtils: boolean;
    function DownloadFromSVN(ModuleName:string;var BeforeRevision, AfterRevision: string; UpdateWarnings: TStringList):boolean;
    function DownloadSVN: boolean;
    procedure DumpOutput(Sender:TProcessEx; output:string);
    // Looks for SVN client in subdirectories and sets FSVNClient.SVNExecutable if found.
    function FindSVNSubDirs(): boolean;
    // Finds compiler in fpcdir path if TFPCInstaller descendant
    function GetCompiler: string;
    function GetCrossInstaller: TCrossInstaller;
    function GetFPCTarget(Native:boolean): string;
    procedure LogError(Sender:TProcessEx;IsException:boolean);
    procedure SetPath(NewPath:string; Prepend:boolean);
  public
    // base directory for installation (fpcdir, lazdir,... option)
    property BaseDirectory: string write FBaseDirectory;
    // compiler to use for building. Specify empty string when using bootstrap compiler.
    property Compiler: string read GetCompiler write FCompiler;
    // Compiler options passed on to make as OPT=
    property CompilerOptions: string write FCompilerOptions;
    // CPU for the target
    property CrossCPU_Target:string write FCrossCPU_Target;
    // OS for target
    property CrossOS_Target:string write FCrossOS_Target;
    // SVN revision override. Default is trunk
    property DesiredRevision:string write FDesiredRevision;
    // Open handle to the logfile
    property LogFile:Text write FLogFile;
    property MakeDirectory:string write FMakeDir;
    // URL for download. HTTP,ftp or svn
    property URL: string write FURL;
    // display and log in temp log file all sub process output
    property Verbose:boolean write FVerbose;
    // write verbatim to log and eventually console
    procedure WriteLog(msg:string;ToConsole:boolean=true);
    // append line ending and write to log and eventually console
    procedure WritelnLog(msg:string;ToConsole:boolean=true);
    // Build module
    function BuildModule(ModuleName:string): boolean; virtual;abstract;
    // Clean up environment
    function CleanModule(ModuleName:string): boolean; virtual;abstract;
    // Config  module
    function ConfigModule(ModuleName:string): boolean; virtual;abstract;
    function GetCompilerInDir(Dir:string):string;
    // Install update sources
    function GetModule(ModuleName:string): boolean; virtual;abstract;
    // Uninstall module
    function UnInstallModule(ModuleName:string): boolean; virtual;abstract;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses installerfpc,fileutil,fpcuputil;

{ TInstaller }

function TInstaller.GetCompiler: string;
begin
  if (Self is TFPCNativeInstaller) or (Self is TFPCInstaller) then
    result:=GetCompilerInDir(FBaseDirectory)
  else
    result:=FCompiler
end;

function TInstaller.GetCrossInstaller: TCrossInstaller;
var
  idx:integer;
  target:string;
begin
  result:=nil;
  target:=GetFPCTarget(false);
  if assigned(CrossInstallers) then
    for idx:=0 to CrossInstallers.Count-1 do
      if CrossInstallers[idx]=target then
        begin
        result:=TCrossInstaller(CrossInstallers.Objects[idx]);
        break;
        end;
end;

function TInstaller.GetMake: string;
begin
  if FMake='' then
    {$IFDEF MSWINDOWS}
    // Make sure there's a trailing delimiter
    FMake:=IncludeTrailingPathDelimiter(FMakeDir)+'make'+GetExeExt;
    {$ELSE}
    FMake:='make'; //assume in path
    {$ENDIF MSWINDOWS}
  result:=FMake;
end;

function TInstaller.CheckAndGetNeededExecutables: boolean;
var
  OperationSucceeded: boolean;
  Output: string;
begin
  OperationSucceeded := True;
  // The extractors used depend on the bootstrap compiler URL/file we download
  // todo: adapt extractor based on URL that's being passed (low priority as these will be pretty stable)
  {$IFDEF MSWINDOWS}
  // Need to do it here so we can pick up make path.
  FBunzip2:=EmptyStr;
  FTar:=EmptyStr;
  // By doing this, we expect unzip.exe to be in the binutils dir.
  // This is safe to do because it is included in the FPC binutils.
  FUnzip := IncludeTrailingPathDelimiter(FMakeDir) + 'unzip' + GetExeExt;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  FBunzip2:='bunzip2';
  FTar:='tar';
  FUnzip:='unzip'; //unzip needed at least for FPC chm help
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  FBunzip2:=''; //not really necessary now
  FTar:='gnutar'; //gnutar can decompress as well; bsd tar can't
  FUnzip:='unzip'; //unzip needed at least for FPC chm help
  {$ENDIF DARIN}

  {$IFDEF MSWINDOWS}
  if OperationSucceeded then
  begin
    // Check for binutils directory, make and unzip executables.
    // Download if needed; will download unzip - needed for SVN download
    if (DirectoryExists(FMakeDir) = False) or (FileExists(Make) = False) or
      (FileExists(FUnzip) = False) then
    begin
      infoln('Make path ' + FMakeDir + ' doesn''t have binutils. Going to download');
      OperationSucceeded := DownloadBinUtils;
    end;
  end;
  {$ENDIF MSWINDOWS}


  if OperationSucceeded then
  begin
    // Check for proper make executable
    try
      ExecuteCommandHidden(Make,'-v',Output,FVerbose);
      if Ansipos('GNU Make', Output) = 0 then
      begin
        infoln('Found make executable but it is not GNU Make.');
        OperationSucceeded:=false;
      end;
    except
      // ignore errors, this is only an extra check
    end;
  end;

  if OperationSucceeded then
  begin
    // Try to look for SVN
    if FSVNClient.FindSVNExecutable='' then
    begin
      {$IFDEF MSWINDOWS}
      // Make sure we have a sensible default.
      // Set it here so multiple calls to CheckExes will not redownload SVN all the time
      if FSVNDirectory='' then FSVNDirectory := IncludeTrailingPathDelimiter(FMakeDir)+'svn'+DirectorySeparator;
      {$ENDIF MSWINDOWS}
      FindSVNSubDirs; //Find svn in or below FSVNDirectory; will also set Updater's SVN executable
      {$IFDEF MSWINDOWS}
      // If it still can't be found, download it
      if FSVNClient.SVNExecutable='' then
      begin
        infoln('Going to download SVN');
        OperationSucceeded := DownloadSVN;
      end;
      {$ELSE}
      if FSVNClient.SVNExecutable='' then
      begin
        infoln('Error: could not find SVN executable. Please make sure it is installed.');
        OperationSucceeded:=false;
      end;
      {$ENDIF}
    end;
  end;

  if OperationSucceeded then
  begin
    // Check for valid unzip executable, if it is needed
    if FUnzip<>EmptyStr then
    begin
      OperationSucceeded:=CheckExecutable(FUnzip, '-v', '');
    end;
  end;

  if OperationSucceeded then
  begin
    // Check for valid bunzip2 executable, if it is needed
    if FBunzip2 <>EmptyStr then
    begin
      OperationSucceeded:=CheckExecutable(FBunzip2, '--version','');
    end;
  end;

  if OperationSucceeded then
  begin
    // Check for valid tar executable, if it is needed
    if FTar<>EmptyStr then
    begin
      OperationSucceeded:=CheckExecutable(FTar, '--version','');
    end;
  end;
  Result := OperationSucceeded;
end;

function TInstaller.CheckExecutable(Executable, Parameters, ExpectOutput: string
  ): boolean;
var
  ResultCode: longint;
  OperationSucceeded: boolean;
  ExeName: string;
  Output: string;
begin
  try
    ExeName:=ExtractFileName(Executable);
    ResultCode:=ExecuteCommandHidden(Executable, Parameters, Output, FVerbose);
    if ResultCode=0 then
    begin
      if (ExpectOutput<>'') and (Ansipos(ExpectOutput, Output)=0) then
      begin
        infoln('Error: '+Executable+' is not a valid '+ExeName+' application. '+
          ExeName+' exists but shows no ('+ExpectOutput+')in its output.');
        OperationSucceeded:=false
      end
      else
      begin
        OperationSucceeded:=true;
      end;
    end
    else
    begin
      infoln('Error: '+Executable+' is not a valid '+ExeName+' application ('+
      ExeName+' result code was: '+IntToStr(ResultCode)+')');
      OperationSucceeded:=false;
    end;
  except
    on E: Exception do
    begin
      infoln('Error: '+Executable+' is not a valid '+ExeName+' application ('+
        'Exception: '+E.ClassName+'/'+E.Message+')');
      OperationSucceeded := False;
    end;
  end;
  if OperationSucceeded then infoln('Found valid '+ExeName+' application.');
  Result:=OperationSucceeded;
end;

procedure TInstaller.CreateBinutilsList;
// Windows-centric for now; doubt if it
// can be used in Unixy systems anyway
begin
  // We need GetExeExt to be defined first.
  FBinUtils:=TStringList.Create;
  FBinUtils.Add('GoRC'+GetExeExt);
  FBinUtils.Add('ar'+GetExeExt);
  FBinUtils.Add('as'+GetExeExt);
  FBinUtils.Add('bin2obj'+GetExeExt);
  FBinUtils.Add('cmp'+GetExeExt);
  FBinUtils.Add('cp'+GetExeExt);
  FBinUtils.Add('cpp.exe');
  FBinUtils.Add('cygiconv-2.dll');
  FBinUtils.Add('cygncurses-8.dll');
  FBinUtils.Add('cygwin1.dll');
  FBinUtils.Add('diff'+GetExeExt);
  FBinUtils.Add('dlltool'+GetExeExt);
  FBinUtils.Add('fp32.ico');
  FBinUtils.Add('gcc'+GetExeExt);
  FBinUtils.Add('gdate'+GetExeExt);
  //GDB.exe apparently can also be found here:
  //http://svn.freepascal.org/svn/lazarus/binaries/i386-win32/gdb/bin/
  //for Windows x64:
  //http://svn.freepascal.org/svn/lazarus/binaries/x86_64-win64/gdb/bin/
  FBinUtils.Add('gdb'+GetExeExt);
  FBinUtils.Add('gecho'+GetExeExt);
  FBinUtils.Add('ginstall'+GetExeExt);
  FBinUtils.Add('ginstall.exe.manifest');
  FBinUtils.Add('gmkdir'+GetExeExt);
  FBinUtils.Add('grep'+GetExeExt);
  FBinUtils.Add('ld'+GetExeExt);
  FBinUtils.Add('libexpat-1.dll');
  FBinUtils.Add('make'+GetExeExt);
  FBinUtils.Add('mv'+GetExeExt);
  FBinUtils.Add('objdump'+GetExeExt);
  FBinUtils.Add('patch'+GetExeExt);
  FBinUtils.Add('patch.exe.manifest');
  FBinUtils.Add('pwd'+GetExeExt);
  FBinUtils.Add('rm'+GetExeExt);
  FBinUtils.Add('strip'+GetExeExt);
  FBinUtils.Add('unzip'+GetExeExt);
  //We might just use gecho for that but that would probably confuse people:
  FBinUtils.Add('upx'+GetExeExt);
  FBinUtils.Add('windres'+GetExeExt);
  FBinUtils.Add('windres'+GetExeExt);
  FBinUtils.Add('zip'+GetExeExt);
end;

procedure TInstaller.CreateStoreSVNDiff(DiffFileName: string;
  UpdateWarnings: TStringList);
var
  DiffFile:Text;
begin
  While FileExists(DiffFileName) do
    DiffFileName:=DiffFileName+'f';
  AssignFile(DiffFile,DiffFileName);
  Rewrite(DiffFile);
  Write(DiffFile,FSVNClient.GetDiffAll);
  CloseFile(DiffFile);
  UpdateWarnings.Add('Diff with last revision stored in '+DiffFileName);
end;

function TInstaller.DownloadBinUtils: boolean;
// Download binutils. For now, only makes sense on Windows...
const
  {These would be the latest:
  SourceUrl = 'http://svn.freepascal.org/svn/fpcbuild/trunk/install/binw32/';
  These might work but are development, too (might end up in 2.6.2):
  SourceUrl = 'http://svn.freepascal.org/svn/fpcbuild/branches/fixes_2_6/install/binw32/';
  but let's use a stable version:}
  SourceURL = 'http://svn.freepascal.org/svn/fpcbuild/tags/release_2_6_0/install/binw32/';
  //Parent directory of files. Needs trailing backslash.
var
  Counter: integer;
  Errors: integer=0;
begin
  ForceDirectoriesUTF8(FMakeDir);
  Result:=true;
  for Counter := 0 to FBinUtils.Count - 1 do
  begin
    infoln('Downloading: ' + FBinUtils[Counter] + ' into ' + FMakeDir);
    try
      if Download(SourceUrl + FBinUtils[Counter], FMakeDir + FBinUtils[Counter])=false then
      begin
        Errors:=Errors+1;
        infoln('Error downloading binutils: '+FBinUtils[Counter]+' to '+FMakeDir);
      end;
    except
      on E: Exception do
      begin
        Result := False;
        infoln('Error downloading binutils: ' + E.Message);
        exit; //out of function.
      end;
    end;
  end;
  if Errors>0 then result:=false;
end;

function TInstaller.DownloadFromSVN(ModuleName: string; var BeforeRevision,
  AfterRevision: string; UpdateWarnings: TStringList): boolean;
begin
  BeforeRevision:='failure';
  AfterRevision:='failure';
  FSVNClient.LocalRepository := FBaseDirectory;
  FSVNClient.Repository := FURL;
  BeforeRevision:=IntToStr(FSVNClient.LocalRevision);
  FSVNClient.LocalModifications(UpdateWarnings); //Get list of modified files
  if UpdateWarnings.Count>0 then
    begin
    CreateStoreSVNDiff(IncludeTrailingPathDelimiter(FBaseDirectory)+'REV'+BeforeRevision+'.diff',UpdateWarnings);
    UpdateWarnings.Insert(0,ModuleName+': WARNING: found modified files.');
    UpdateWarnings.Add(ModuleName+': reverting before updating.');
    FSVNClient.Revert; //Remove local changes
    end;
  FSVNClient.DesiredRevision:=FDesiredRevision; //Desired revision
  FSVNClient.CheckOutOrUpdate;
  AfterRevision:=IntToStr(FSVNClient.LocalRevision);
  if BeforeRevision<>AfterRevision then FSVNUpdated:=true else FSVNUpdated:=false;
  Result := True;
end;

function TInstaller.DownloadSVN: boolean;
var
  OperationSucceeded: boolean;
  ResultCode: longint;
  SVNZip: string;
begin
  // Download SVN in make path. Not required for making FPC/Lazarus, but when downloading FPC/Lazarus from... SVN ;)
  { Alternative 1: sourceforge packaged
  This won't work, we'd get an .msi:
  http://sourceforge.net/projects/win32svn/files/latest/download?source=files
  We don't want msi/Windows installer - this way we can hopefully support Windows 2000, so use:
  http://heanet.dl.sourceforge.net/project/win32svn/1.7.2/svn-win32-1.7.2.zip
  }

  {Alternative 2: use
  http://www.visualsvn.com/files/Apache-Subversion-1.7.2.zip
  with subdirs bin and licenses. No further subdirs
  However, doesn't work on Windows 2K...}
  OperationSucceeded := True;
  ForceDirectoriesUTF8(FSVNDirectory);
  SVNZip := SysUtils.GetTempFileName + '.zip';
  try
    OperationSucceeded := Download(
      'http://heanet.dl.sourceforge.net/project/win32svn/1.7.2/svn-win32-1.7.2.zip',
      SVNZip);
  except
    // Deal with timeouts, wrong URLs etc
    OperationSucceeded:=false;
  end;

  if OperationSucceeded then
  begin
    // Extract, overwrite
    if ExecuteCommandHidden(FUnzip,'-o -d '+ FSVNDirectory+' '+SVNZip,FVerbose)<> 0 then
      begin
        OperationSucceeded := False;
        infoln('resultcode: ' + IntToStr(ResultCode));
      end;
  end;

  if OperationSucceeded then
  begin
    OperationSucceeded := FindSVNSubDirs;
    if OperationSucceeded then
      SysUtils.deletefile(SVNZip); //Get rid of temp zip if success.
  end;
  Result := OperationSucceeded;
end;

procedure TInstaller.DumpOutput(Sender: TProcessEx; output: string);
var
  TempFileName:string;
begin
  if FVerbose then
    begin
    if TextRec(FLogVerboseFile).Mode=0 then
      begin
      TempFileName:=SysUtils.GetTempFileName;
      AssignFile(FLogVerboseFile,TempFileName);
      Rewrite(FLogVerboseFile);
      WritelnLog('Verbose output saved to '+TempFileName,false);
      end;
    write(FLogVerboseFile,output);
    end;
  DumpConsole(Sender,output);
end;

function TInstaller.FindSVNSubDirs: boolean;
var
  SVNFiles: TStringList;
  OperationSucceeded: boolean;
begin
  //SVNFiles:=TStringList.Create; //No, Findallfiles does that for you!?!?
  SVNFiles := FindAllFiles(FSVNDirectory, 'svn' + GetExeExt, True);
  try
    if SVNFiles.Count > 0 then
    begin
      // Just get first result.
      FSVNClient.SVNExecutable := SVNFiles.Strings[0];
      OperationSucceeded := True;
    end
    else
    begin
      infoln('Could not find svn executable in or under ' + FSVNDirectory);
      OperationSucceeded := False;
    end;
  finally
    SVNFiles.Free;
  end;
  Result := OperationSucceeded;
end;


function TInstaller.GetFPCTarget(Native: boolean): string;
var
  processorname,os:string;
begin
  processorname:='notfound';
  os:=processorname;
  {$ifdef cpui386}
       processorname:='i386';
  {$endif cpui386}
  {$ifdef cpum68k}
       processorname:='m68k';
  {$endif cpum68k}
  {$ifdef cpualpha}
       processorname:='alpha';
  {$endif cpualpha}
  {$ifdef cpupowerpc}
       processorname:='powerpc';
  {$endif cpupowerpc}
  {$ifdef cpupowerpc64}
       processorname:='powerpc64';
  {$endif cpupowerpc64}
  {$ifdef cpuarm}
    {$ifdef fpc_armeb}
       processorname:='armeb';
    {$else}
       processorname:='arm';
    {$endif fpc_armeb}
  {$endif cpuarm}
  {$ifdef cpusparc}
       processorname:='sparc';
  {$endif cpusparc}
  {$ifdef cpux86_64}
       processorname:='x86_64';
  {$endif cpux86_64}
  {$ifdef cpuia64}
       processorname:='ia64';
  {$endif cpuia64}
  {$ifdef darwin}
       os:='darwin';
  {$endif darwin}
  {$ifdef FreeBSD}
       os:='freebsd';
  {$endif FreeBSD}
  {$ifdef linux}
       os:='linux';
  {$endif linux}
  {$ifdef netbsd}
       os:='netbsd';
  {$endif netbsd}
  {$ifdef openbsd}
       os:='openbsd';
  {$endif openbsd}
  {$ifdef os2}
       os:='os2';
  {$endif os2}
  {$ifdef solaris}
       os:='solaris';
  {$endif solaris}
  {$ifdef wince}
       os:='wince';
  {$endif wince}
  {$ifdef win32}
       os:='win32';
  {$endif win32}
  {$ifdef win64}
       os:='win64';
  {$endif win64}
  if not Native then
    begin
    if FCrossCPU_Target<>'' then
      processorname:= FCrossCPU_Target;
    if FCrossOS_Target<>'' then
      os:=FCrossOS_Target;
    end;
  result:=processorname+'-'+os;
end;

procedure TInstaller.LogError(Sender: TProcessEx; IsException: boolean);
var
  TempFileName:string;
begin
  TempFileName:=SysUtils.GetTempFileName;
  if IsException then
    begin
    WritelnLog('Exception raised running ' + Sender.Executable + ' ' +Sender.ParametersString, true);
    WritelnLog(Sender.ExceptionInfo, true);
    end
  else
    begin
    infoln('Command returned non-zero ExitStatus: '+IntToStr(Sender.ExitStatus)+'. Output:');
    infoln(Sender.OutputString);
    WritelnLog('ERROR running '+Sender.Executable + ' ' +Sender.ParametersString,false);
    Sender.OutputStrings.SaveToFile(TempFileName);
    WritelnLog('  output logged in '+TempFileName,false);
    end;
end;

procedure TInstaller.SetPath(NewPath: string; Prepend:boolean);
begin
  if Prepend then
  {$IFDEF MSWINDOWS}
    NewPath:=NewPath+PathSeparator+ProcessEx.Environment.GetVar('Path');
  ProcessEx.Environment.SetVar('Path',NewPath);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
    NewPath:=NewPath+PathSeparator+ProcessEx.Environment.GetVar('PATH');
  ProcessEx.Environment.SetVar('PATH',NewPath);
  {$ENDIF UNIX}
  if NewPath<>EmptyStr then
    WritelnLog('External program path:  '+NewPath,false);
end;

procedure TInstaller.WriteLog(msg: string; ToConsole: boolean);
begin
  Write(FLogFile,msg);
  if ToConsole then
    InfoLn(msg);
end;

procedure TInstaller.WritelnLog(msg: string; ToConsole: boolean);
begin
  WriteLog(msg+LineEnding,false); //infoln adds already a lf
  if ToConsole then
    InfoLn(msg);
end;

function TInstaller.GetCompilerInDir(Dir: string): string;
var
  ExeName:string;
begin
  ExeName:='fpc'+GetExeExt;
  result :=  IncludeTrailingBackslash(Dir) + 'bin' +DirectorySeparator+
    GetFPCTarget(true)+DirectorySeparator+'fpc';
  {$IFDEF UNIX}
  if FileExistsUTF8(result+'.sh') then
  begin
    //Use our proxy if it is installed
    result:=result+'.sh';
  end;
  {$ENDIF UNIX}
end;

constructor TInstaller.Create;
begin
  inherited create;
  ProcessEx:=TProcessEx.Create(nil);
  ProcessEx.OnErrorM:=@LogError;
  FSVNClient:=TSVNClient.Create;
end;

destructor TInstaller.Destroy;
begin
  if Assigned(FBinUtils) then
    FBinUtils.Free;
  ProcessEx.Free;
  FSVNClient.Free;
  inherited Destroy;
end;


end.

