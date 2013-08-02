unit installerCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  GitClient, HGClient, SvnClient,
  processutils, m_crossinstaller, fpcuputil;

type

  { TInstaller }

  TInstaller = class(TObject)
  private
    FKeepLocalChanges: boolean;
    FPatchCmd: string;
    FReApplyLocalChanges: boolean;
    function GetMake: string;
  protected
    FBaseDirectory: string; //Top directory for a product (FPC, Lazarus)
    FBinUtils: TStringList; //List of executables such as make.exe needed for compilation on Win32
    FBunzip2: string;
    FCompiler: string; // Compiler executable
    FCompilerOptions: string;
    FCrossCPU_Target: string; //When cross-compiling: CPU, e.g. x86_64
    FCrossOS_Target: string; //When cross-compiling: OS, e.g. win64
    FDesiredRevision: string;
    // Stores tprocessex exception info:
    FErrorLog: TStringList;
    FGitClient: TGitClient;
    FHGClient: THGClient;
    FLog: TLogger;
    FLogVerbose: TLogger; // Log file separate from main fpcup.log, for verbose logging
    FMake: string;
    FMakeDir: string;
    FNeededExecutablesChecked: boolean;
    FSVNClient: TSVNClient;
    FSVNDirectory: string;
    FRepositoryUpdated: boolean;
    FURL: string;
    FVerbose: boolean;
    FTar: string;
    FUnzip: string;
    ProcessEx: TProcessEx;
    property Make: string read GetMake;
    // Check for existence of required executables; if not there, get them if possible
    function CheckAndGetNeededExecutables: boolean;
    // Check executable is the right one: run Executable with Parameters and look for ExpectOutput. Returns false if no match.
    function CheckExecutable(Executable, Parameters, ExpectOutput: string): boolean;
    // Make a list of all binutils that can be downloaded
    procedure CreateBinutilsList;
    // Get a diff of all modified files in and below the directory and save it
    procedure CreateStoreRepositoryDiff(DiffFileName: string; UpdateWarnings: TStringList; RepoClass: TObject);
    // Download make.exe, unzip.exe etc into the make directory (only implemented for Windows):
    function DownloadBinUtils: boolean;
    // Clone/update using HG; use FBaseDirectory as local repository
    function DownloadFromHG(ModuleName: string; var BeforeRevision, AfterRevision: string; UpdateWarnings: TStringList): boolean;
    // Clone/update using Git; use FBaseDirectory as local repository
    function DownloadFromGit(ModuleName: string; var BeforeRevision, AfterRevision: string; UpdateWarnings: TStringList): boolean;
    // Checkout/update using SVN; use FBaseDirectory as local repository
    function DownloadFromSVN(ModuleName: string; var BeforeRevision, AfterRevision: string; UpdateWarnings: TStringList): boolean;
    // Download SVN client and set FSVNClient.SVNExecutable if succesful.
    function DownloadSVN: boolean;
    procedure DumpOutput(Sender: TProcessEx; output: string);
    // Looks for SVN client in subdirectories and sets FSVNClient.SVNExecutable if found.
    function FindSVNSubDirs(): boolean;
    // Finds compiler in fpcdir path if TFPCInstaller descendant
    function GetCompiler: string;
    function GetCrossInstaller: TCrossInstaller;
    // Returns CPU-OS in the format used by the FPC bin directory, e.g. x86_64-win64:
    function GetFPCTarget(Native: boolean): string;
    procedure LogError(Sender: TProcessEx; IsException: boolean);
    // Sets the search/binary path to NewPath or adds NewPath to path:
    procedure SetPath(NewPath: string; Prepend: boolean);
  public
    // Get processerrors and put them into FErrorLog
    procedure ProcessError(Sender:TProcessEx;IsException:boolean);
    // base directory for installation (fpcdir, lazdir,... option)
    property BaseDirectory: string write FBaseDirectory;
    // compiler to use for building. Specify empty string when using bootstrap compiler.
    property Compiler: string read GetCompiler write FCompiler;
    // Compiler options passed on to make as OPT=
    property CompilerOptions: string write FCompilerOptions;
    // CPU for the target (together with CrossOS_Target the cross compile equivalent to GetFPCTarget)
    property CrossCPU_Target: string read FCrossCPU_Target write FCrossCPU_Target;
    // OS for target (together with CrossCPU_Target the cross compile equivalent to GetFPCTarget)
    property CrossOS_Target: string read FCrossOS_Target write FCrossOS_Target;
    // SVN revision override. Default is HEAD/latest revision
    property DesiredRevision: string write FDesiredRevision;
    // Whether or not to let locally modified files remain or back them up to .diff and svn revert before compiling
    property KeepLocalChanges: boolean write FKeepLocalChanges;
    property Log: TLogger write FLog;
    property MakeDirectory: string write FMakeDir;
    // Patch utility to use. Defaults to 'patch -p0 -i patchfile'
    property PatchCmd:string write FPatchCmd;
    // Whether or not to back up locale changes to .diff and reapply them before compiling
    property ReApplyLocalChanges: boolean write FReApplyLocalChanges;
    // URL for download. HTTP,ftp or svn
    property URL: string write FURL;
    // display and log in temp log file all sub process output
    property Verbose: boolean write FVerbose;
    // append line ending and write to log and, if specified, to console
    procedure WritelnLog(msg: string; ToConsole: boolean = true);
    // Build module
    function BuildModule(ModuleName: string): boolean; virtual; abstract;
    // Clean up environment
    function CleanModule(ModuleName: string): boolean; virtual; abstract;
    // Config module
    function ConfigModule(ModuleName: string): boolean; virtual; abstract;
    // Constructs compiler path from directory and architecture
    // Corrects for use of our fpc.sh launcher on *nix
    // Does not verify compiler actually exists.
    function GetCompilerInDir(Dir: string): string;
    // Install update sources
    function GetModule(ModuleName: string): boolean; virtual; abstract;
    // Uninstall module
    function UnInstallModule(ModuleName: string): boolean; virtual; abstract;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses installerfpc, fileutil;

const
  {$IFDEF MSWINDOWS}
  PATHVARNAME = 'Path';
  {$ELSE}
  //Unix/Linux
  PATHVARNAME = 'PATH';
  {$ENDIF MSWINDOWS}

{ TInstaller }

function TInstaller.GetCompiler: string;
begin
  if (Self is TFPCNativeInstaller) or (Self is TFPCInstaller) then
    Result := GetCompilerInDir(FBaseDirectory)
  else
    Result := FCompiler;
end;

function TInstaller.GetCrossInstaller: TCrossInstaller;
var
  idx: integer;
  target: string;
begin
  Result := nil;
  target := GetFPCTarget(false);
  if assigned(CrossInstallers) then
    for idx := 0 to CrossInstallers.Count - 1 do
      if CrossInstallers[idx] = target then
      begin
        Result := TCrossInstaller(CrossInstallers.Objects[idx]);
        break;
      end;
end;

function TInstaller.GetMake: string;
begin
  if FMake = '' then
    {$IFDEF MSWINDOWS}
    FMake := IncludeTrailingPathDelimiter(FMakeDir) + 'make' + GetExeExt;
    {$ELSE}
    {$IF defined(BSD) and not defined(DARWIN)}
    FMake := 'gmake'; //GNU make; assume in path
    {$else}
    // Linux, OSX
    FMake := 'make'; //assume in path
    {$ENDIF}
    {$ENDIF MSWINDOWS}
  Result := FMake;
end;

function TInstaller.CheckAndGetNeededExecutables: boolean;
var
  AllThere: boolean;
  i: integer;
  OperationSucceeded: boolean;
  Output: string;
begin
  OperationSucceeded := true;
  if not FNeededExecutablesChecked then
  begin
    // The extractors used depend on the bootstrap compiler URL/file we download
    // todo: adapt extractor based on URL that's being passed (low priority as these will be pretty stable)
    {$IFDEF MSWINDOWS}
    // Need to do it here so we can pick up make path.
    FBunzip2 := '';
    FTar := '';
    // By doing this, we expect unzip.exe to be in the binutils dir.
    // This is safe to do because it is included in the FPC binutils.
    FUnzip := IncludeTrailingPathDelimiter(FMakeDir) + 'unzip' + GetExeExt;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    FBunzip2 := 'bunzip2';
    FTar := 'tar';
    FUnzip := 'unzip'; //unzip needed at least for FPC chm help
    {$ENDIF LINUX}
    {$IFDEF BSD} //OSX, *BSD
    {$IFDEF DARWIN}
    FBunzip2 := ''; //not really necessary now
    FTar := 'gnutar'; //gnutar can decompress as well; bsd tar can't
    FUnzip := 'unzip'; //unzip needed at least for FPC chm help
    {$ELSE} //FreeBSD, OpenBSD, NetBSD
    FBunzip2 := 'bunzip2';
    FTar := 'tar'; //At least FreeBSD tar apparently takes some gnu tar options nowadays.
    FUnzip := 'unzip'; //unzip needed at least for FPC chm help
    {$ENDIF DARWIN}
    {$ENDIF BSD}

    {$IFDEF MSWINDOWS}
    if OperationSucceeded then
    begin
      // Download if needed, including unzip - needed for SVN download
      // Check for binutils directory
      AllThere:=true;
      if DirectoryExists(FMakeDir) = false then
      begin
        infoln('Make path ' + FMakeDir + ' does not exist. Going to download binutils.',etinfo);
        AllThere:=false;
      end
      else
      begin
        // Check all binutils in directory
        for i:=0 to FBinUtils.Count-1 do
        begin
          if not(FileExists(IncludeTrailingPathDelimiter(FMakeDir)+FBinUtils[i])) then
          begin
            AllThere:=false;
            infoln('Make path ' + FMakeDir + ' does not have (all) binutils. Going to download binutils.',etinfo);
            break;
          end;
        end;
      end;
      if not(AllThere) then OperationSucceeded := DownloadBinUtils;
    end;
    {$ENDIF MSWINDOWS}
    {$IF defined(LINUX) or (defined(BSD) and (not defined(DARWIN)))} //Linux,FreeBSD,NetBSD,OpenBSD, but not OSX
    //todo: check if we need as on OSX as well
    if OperationSucceeded then
    begin
      // Check for proper assembler
      try
        if ExecuteCommand('as --version', Output, FVerbose) <> 0 then
        begin
          infoln('Missing assembler as. Please install the developer tools.',eterror);
          OperationSucceeded := false;
        end;
      except
        OperationSucceeded := false;
        // ignore errors, this is only an extra check
      end;
    end;
    {$ENDIF defined(LINUX) or (defined(BSD) and (not defined(DARWIN)))}

    if OperationSucceeded then
    begin
      // Check for proper make executable
      try
        ExecuteCommand(Make + ' -v', Output, FVerbose);
        if Ansipos('GNU Make', Output) = 0 then
        begin
          infoln('Found make executable but it is not GNU Make.',etwarning);
          OperationSucceeded := false;
        end;
      except
        // ignore errors, this is only an extra check
      end;
    end;

    if OperationSucceeded then
    begin
      // Look for SVN executable and set it if found:
      if FSVNClient.FindRepoExecutable = '' then
      begin
        {$IFDEF MSWINDOWS}
        // Make sure we have a sensible default.
        // Set it here so multiple calls will not redownload SVN all the time
        if FSVNDirectory = '' then
          FSVNDirectory := IncludeTrailingPathDelimiter(FMakeDir) + 'svn' + DirectorySeparator;
        {$ENDIF MSWINDOWS}
        // Look in or below FSVNDirectory; will set FSVNClient.SVNExecutable
        FindSVNSubDirs;
        {$IFDEF MSWINDOWS}
        // If it still can't be found, download it
        if FSVNClient.RepoExecutable = '' then
        begin
          infoln('Going to download SVN',etinfo);
          // Download will look in and below FSVNDirectory
          // and set FSVNClient.SVNExecutable if succesful
          OperationSucceeded := DownloadSVN;
        end;
        {$ENDIF}

        // Regardless of platform, SVN should now be either set up correctly or we should give up.
        if FSVNClient.RepoExecutable = '' then
        begin
          infoln('Could not find SVN executable. Please make sure it is installed.',eterror);
          OperationSucceeded := false;
        end;
      end;
    end;

    if OperationSucceeded then
    begin
      // Check for valid unzip executable, if it is needed
      if FUnzip <> '' then
      begin
        {$IF (defined(BSD)) and (not defined(Darwin))}
        // FreeBSD doesn't have an unzip applicationt that responds without needing a zip file
        // No motivation to go feed it a zip file just to test it
        OperationSucceeded := true;
        {$ELSE}
        // OSes with a normal unzip
        OperationSucceeded := CheckExecutable(FUnzip, '-v', '');
        {$ENDIF (defined(BSD)) and (not defined(Darwin))}
      end;
    end;

    if OperationSucceeded then
    begin
      // Check for valid bunzip2 executable, if it is needed
      if FBunzip2 <> EmptyStr then
      begin
        { Used to use bunzip2 --version, but on e.g. Fedora Core
        that returns an error message e.g. can't read from cp
        }
        OperationSucceeded := CheckExecutable(FBunzip2, '--help', '');
      end;
    end;

    if OperationSucceeded then
    begin
      // Check for valid tar executable, if it is needed
      if FTar <> EmptyStr then
      begin
        OperationSucceeded := CheckExecutable(FTar, '--version', '');
      end;
    end;
    FNeededExecutablesChecked:=OperationSucceeded;
  end;
  Result := OperationSucceeded;
end;

function TInstaller.CheckExecutable(Executable, Parameters, ExpectOutput: string): boolean;
var
  ResultCode: longint;
  OperationSucceeded: boolean;
  ExeName: string;
  Output: string;
begin
  try
    ExeName := ExtractFileName(Executable);
    ResultCode := ExecuteCommand(Executable + ' ' + Parameters, Output, FVerbose);
    if ResultCode >= 0 then //Not all non-0 result codes are errors. There's no way to tell, really
    begin
      if (ExpectOutput <> '') and (Ansipos(ExpectOutput, Output) = 0) then
      begin
        // This is not a warning/error message as sometimes we can use multiple different versions of executables
        infoln(Executable + ' is not a valid ' + ExeName + ' application. ' +
          ExeName + ' exists but shows no (' + ExpectOutput + ') in its output.',etinfo);
        OperationSucceeded := false;
      end
      else
      begin
        OperationSucceeded := true;
      end;
    end
    else
    begin
      // This is not a warning/error message as sometimes we can use multiple different versions of executables
      infoln(Executable + ' is not a valid ' + ExeName + ' application (' + ExeName + ' result code was: ' + IntToStr(ResultCode) + ')',etinfo);
      OperationSucceeded := false;
    end;
  except
    on E: Exception do
    begin
      // This is not a warning/error message as sometimes we can use multiple different versions of executables
      infoln(Executable + ' is not a valid ' + ExeName + ' application (' + 'Exception: ' + E.ClassName + '/' + E.Message + ')', etinfo);
      OperationSucceeded := false;
    end;
  end;
  if OperationSucceeded then
    infoln('Found valid ' + ExeName + ' application.',etdebug);
  Result := OperationSucceeded;
end;

procedure TInstaller.CreateBinutilsList;
// Windows-centric for now; doubt if it
// can be used in Unixy systems anyway
begin
  FBinUtils := TStringList.Create;
  FBinUtils.Add('GoRC' + GetExeExt);
  FBinUtils.Add('ar' + GetExeExt);
  FBinUtils.Add('as' + GetExeExt);
  FBinUtils.Add('cmp' + GetExeExt);
  FBinUtils.Add('cp' + GetExeExt);
  FBinUtils.Add('cpp' + GetExeExt);
  FBinUtils.Add('diff' + GetExeExt);
  FBinUtils.Add('dlltool' + GetExeExt);
  FBinUtils.Add('fp32.ico');
  FBinUtils.Add('gcc' + GetExeExt);
  FBinUtils.Add('gdate' + GetExeExt);
  FBinUtils.Add('gdb' + GetExeExt);
  FBinUtils.Add('gecho' + GetExeExt);
  FBinUtils.Add('ginstall' + GetExeExt);
  FBinUtils.Add('ginstall.exe.manifest');
  FBinUtils.Add('gmkdir' + GetExeExt);
  FBinUtils.Add('grep' + GetExeExt);
  FBinUtils.Add('ld' + GetExeExt);
  {$ifdef win32}
  // Debugger support files
  // On changes, please update debugger list in DownloadBinUtils:
  FBinUtils.Add('libexpat-1.dll');
  {
  http://svn.freepascal.org/svn/lazarus/binaries/i386-win32/gdb/bin/
  only has libexpat-1, so no need for these:
  FBinUtils.Add('libgcc_s_dw2-1.dll');
  FBinUtils.Add('libiconv-2.dll');
  FBinUtils.Add('libintl-8.dll');
  }
  {$endif win32}
  {$ifdef win64}
  // Debugger support files:
  // On changes, please update debugger list in DownloadBinUtils:
  FBinUtils.Add('libiconv-2.dll');
  {$endif win64}
  FBinUtils.Add('make' + GetExeExt);
  FBinUtils.Add('mv' + GetExeExt);
  FBinUtils.Add('objdump' + GetExeExt);
  FBinUtils.Add('patch' + GetExeExt);
  FBinUtils.Add('patch.exe.manifest');
  FBinUtils.Add('pwd' + GetExeExt);
  FBinUtils.Add('rm' + GetExeExt);
  FBinUtils.Add('strip' + GetExeExt);
  FBinUtils.Add('unzip' + GetExeExt);
  // As of FPC 2.6.2, UPX is no longer included:
  //FBinUtils.Add('upx' + GetExeExt);
  FBinUtils.Add('windres' + GetExeExt);
  FBinUtils.Add('windres.h');
  FBinUtils.Add('zip' + GetExeExt);
end;

procedure TInstaller.CreateStoreRepositoryDiff(DiffFileName: string; UpdateWarnings: TStringList; RepoClass: TObject);
var
  DiffFile: Text;
begin
  while FileExists(DiffFileName) do
    DiffFileName := DiffFileName + 'f';
  AssignFile(DiffFile, DiffFileName);
  Rewrite(DiffFile);
  if assigned(RepoClass) then
  begin
    if RepoClass is THGClient then
    begin
      Write(DiffFile, FHGClient.GetDiffAll);
    end
    else if RepoClass is TSVNClient then
    begin
      Write(DiffFile, FSVNClient.GetDiffAll);
    end
    else raise Exception.CreateFmt('Error writing diff file. Technical details: unknown repository object %s passed. Please fix the code.',[RepoClass.ClassName]);
  end
  else
  begin
    raise Exception.Create('Error writing diff file. Technical details: invalid/no repository object passed. Please fix the code.');
  end;
  CloseFile(DiffFile);
  UpdateWarnings.Add('Diff with last revision stored in ' + DiffFileName);
end;

function TInstaller.DownloadBinUtils: boolean;
  // Download binutils. For now, only makes sense on Windows...
const
  {These would be the latest:
  SourceUrl = 'http://svn.freepascal.org/svn/fpcbuild/trunk/install/binw32/';
  These might work but are development, too (might end up in 2.6.2):
  SourceUrl = 'http://svn.freepascal.org/svn/fpcbuild/branches/fixes_2_6/install/binw32/';
  but let's use a stable version:}
  SourceURL = 'http://svn.freepascal.org/svn/fpcbuild/tags/release_2_6_2/install/binw32/';
  // For gdb (x64 and x86), we use the Lazarus supplied ones rather than the FPC supplied ones.
  // Lazarus is tightly coupled to gdb versions thanks to Martin Friebe's work with bug fixes
  SourceURL_gdb = 'http://svn.freepascal.org/svn/lazarus/binaries/i386-win32/gdb/bin/';
  SourceURL64 = 'http://svn.freepascal.org/svn/fpcbuild/tags/release_2_6_2/install/binw64/';
  SourceURL64_gdb = 'http://svn.freepascal.org/svn/lazarus/binaries/x86_64-win64/gdb/bin/';
  //todo: add Qt and Qt4Pas5.dll in http://svn.freepascal.org/svn/lazarus/binaries/i386-win32/qt/?
var
  Counter: integer;
  {$ifdef win32}
  DebuggerFiles: TStringList; //Files on the SourceURL_gdb page
  {$endif win32}
  {$ifdef win64}
  Debugger64Files: TStringList; //Files on the SourceURL64_gdb page
  Win64SpecificFiles: TStringList; //Files on the SourceURL64 page
  {$endif win64}
  Errors: integer = 0;
  RootURL: string;
begin
  {$ifdef win32}
  DebuggerFiles:=TStringList.Create;
  {$endif win32}
  {$ifdef win64}
  Debugger64Files:=TStringList.Create;
  Win64SpecificFiles:=TStringList.Create;
  {$endif win64}
  try
    //Parent directory of files. Needs trailing backslash.
    ForceDirectoriesUTF8(FMakeDir);
    Result := true;
    // Set up exception list:
    {$ifdef win32}
    DebuggerFiles.Add('gdb.exe');
    DebuggerFiles.Add('libexpat-1.dll');
    {
    http://svn.freepascal.org/svn/lazarus/binaries/i386-win32/gdb/bin/
    only has libexpat-1, so no need for these:
    DebuggerFiles.Add('libgcc_s_dw2-1.dll');
    DebuggerFiles.Add('libiconv-2.dll');
    DebuggerFiles.Add('libintl-8.dll');
    }
    {$endif win32}
    {$ifdef win64}
    Debugger64Files.Add('gdb.exe');
    Debugger64Files.Add('libiconv-2.dll');
    // These files are from the 64 bit site...
    Win64SpecificFiles.Add('GoRC.exe');
    Win64SpecificFiles.Add('ar.exe');
    Win64SpecificFiles.Add('as.exe');
    Win64SpecificFiles.Add('cmp.exe');
    Win64SpecificFiles.Add('cp.exe');
    Win64SpecificFiles.Add('diff.exe');
    Win64SpecificFiles.Add('gdate.exe');
    Win64SpecificFiles.Add('gecho.exe');
    Win64SpecificFiles.Add('ginstall.exe');
    Win64SpecificFiles.Add('ginstall.exe.manifest');
    Win64SpecificFiles.Add('gmkdir.exe');
    Win64SpecificFiles.Add('ld.exe');
    Win64SpecificFiles.Add('make.exe');
    Win64SpecificFiles.Add('mv.exe');
    Win64SpecificFiles.Add('objdump.exe');
    Win64SpecificFiles.Add('pwd.exe');
    Win64SpecificFiles.Add('rm.exe');
    Win64SpecificFiles.Add('strip.exe');
    // ... the rest will be downloaded from the 32 bit site
    {$endif win64}
    for Counter := 0 to FBinUtils.Count - 1 do
    begin
      infoln('Downloading: ' + FBinUtils[Counter] + ' into ' + FMakeDir,etinfo);
      try
        {$ifdef win32}
        if DebuggerFiles.IndexOf(FBinUtils[Counter])>-1 then
          RootUrl:=SourceURL_gdb
        else
          RootURL:=SourceURL;
        {$endif win32}
        {$ifdef win64}
        // First override with specific locations before falling back
        // to the win32 site
        if Debugger64Files.IndexOf(FBinUtils[Counter])>-1 then
          RootUrl:=SourceURL64_gdb
        else if Win64SpecificFiles.IndexOf(FBinUtils[Counter])>-1 then
          RootURL:=SourceURL64
        else
          RootURL:=SourceURL;
        {$endif win64}
        if Download(RootUrl + FBinUtils[Counter], IncludeTrailingPathDelimiter(FMakeDir) + FBinUtils[Counter]) = false then
        begin
          Errors := Errors + 1;
          infoln('Error downloading binutils: ' + FBinUtils[Counter] + ' to ' + FMakeDir,eterror);
        end;
      except
        on E: Exception do
        begin
          Result := false;
          infoln('Error downloading binutils: ' + E.Message,eterror);
          exit; //out of function.
        end;
      end;
    end;

    if Errors > 0 then
    begin
      Result := false;
      WritelnLog('DownloadBinUtils: ' + IntToStr(Errors) + ' errors downloading binutils.', true);
    end;
  finally
    {$ifdef win32}
    DebuggerFiles.Free;
    {$endif win32}
    {$ifdef win64}
    Debugger64Files.Free;
    Win64SpecificFiles.Free;
    {$endif win64}
  end;
end;

function TInstaller.DownloadFromHG(ModuleName: string; var BeforeRevision,
  AfterRevision: string; UpdateWarnings: TStringList): boolean;
var
  BeforeRevisionShort: string; //Basically the branch revision number
  ReturnCode: integer;
begin
  //todo: check if we need to add forcedirectoriesutf8 to create local repo dir if it doesn't exist
  BeforeRevision := 'failure';
  BeforeRevisionShort:='unknown';
  AfterRevision := 'failure';
  FHGClient.LocalRepository := FBaseDirectory;
  FHGClient.Repository := FURL;

  BeforeRevision := 'revision '+FHGClient.LocalRevision;
  BeforeRevisionShort:=FHGClient.LocalRevision;

  FHGClient.LocalModifications(UpdateWarnings); //Get list of modified files
  if UpdateWarnings.Count > 0 then
  begin
    UpdateWarnings.Insert(0, ModuleName + ': WARNING: found modified files.');
    if FKeepLocalChanges=false then
    begin
      CreateStoreRepositoryDiff(IncludeTrailingPathDelimiter(FBaseDirectory) + 'REV' + BeforeRevisionShort + '.diff', UpdateWarnings,FHGClient);
      UpdateWarnings.Add(ModuleName + ': reverting before updating.');
      FHGClient.Revert; //Remove local changes
    end
    else
    begin
      UpdateWarnings.Add(ModuleName + ': leaving modified files as is before updating.');
    end;
  end;

  FHGClient.DesiredRevision := FDesiredRevision; //We want to update to this specific revision
  // CheckoutOrUpdate sets result code. We'd like to detect e.g. mixed repositories.
  FHGClient.CheckOutOrUpdate;
  ReturnCode := FHGClient.ReturnCode;
  case ReturnCode of
    FRET_LOCAL_REMOTE_URL_NOMATCH:
    begin
      FRepositoryUpdated := false;
      Result := false;
      writelnlog('ERROR: repository URL in local directory and remote repository don''t match.', true);
      writelnlog('Local directory: ' + FHGClient.LocalRepository, true);
      infoln('Have you specified the wrong directory or a directory with an old repository checkout?',etinfo);
    end;
    else
    begin
      // For now, assume it worked even with non-zero result code. We can because
      // we do the AfterRevision check as well.
      AfterRevision := 'revision '+FHGClient.LocalRevision;
      if (FHGClient.LocalRevision<>FRET_UNKNOWN_REVISION) and (BeforeRevisionShort <> FHGClient.LocalRevision) then
        FRepositoryUpdated := true
      else
        FRepositoryUpdated := false;
      Result := true;
    end;
  end;
end;

function TInstaller.DownloadFromGit(ModuleName: string; var BeforeRevision,
  AfterRevision: string; UpdateWarnings: TStringList): boolean;
//todo: look into merging this with DownloadFromHG, DownloadFromSVN, passing a repoclient object
var
  BeforeRevisionShort: string; //Basically the branch revision number
  ReturnCode: integer;
begin
  //todo: check if forcedirectoriesutf8 for local repo is needed
  BeforeRevision := 'failure';
  BeforeRevisionShort:='unknown';
  AfterRevision := 'failure';
  FGitClient.LocalRepository := FBaseDirectory;
  FGitClient.Repository := FURL;

  BeforeRevision := 'revision '+FGitClient.LocalRevision;
  BeforeRevisionShort:=FGitClient.LocalRevision;

  if BeforeRevisionShort<>FRET_UNKNOWN_REVISION then
  begin
    FGitClient.LocalModifications(UpdateWarnings); //Get list of modified files
    if UpdateWarnings.Count > 0 then
    begin
      UpdateWarnings.Insert(0, ModuleName + ': WARNING: found modified files.');
      if FKeepLocalChanges=false then
      begin
        CreateStoreRepositoryDiff(IncludeTrailingPathDelimiter(FBaseDirectory) + 'REV' + BeforeRevisionShort + '.diff', UpdateWarnings,FGitClient);
        UpdateWarnings.Add(ModuleName + ': reverting before updating.');
        FGitClient.Revert; //Remove local changes
      end
      else
      begin
        UpdateWarnings.Add(ModuleName + ': leaving modified files as is before updating.');
      end;
    end;
  end;

  FGitClient.DesiredRevision := FDesiredRevision; //We want to update to this specific revision
  // CheckoutOrUpdate sets result code. We'd like to detect e.g. mixed repositories.
  FGitClient.CheckOutOrUpdate;
  ReturnCode := FGitClient.ReturnCode;
  case ReturnCode of
    FRET_LOCAL_REMOTE_URL_NOMATCH:
    begin
      FRepositoryUpdated := false;
      Result := false;
      writelnlog('ERROR: repository URL in local directory and remote repository don''t match.', true);
      writelnlog('Local directory: ' + FGitClient.LocalRepository, true);
      infoln('Have you specified the wrong directory or a directory with an old repository checkout?',etinfo);
    end;
    else
    begin
      // For now, assume it worked even with non-zero result code. We can because
      // we do the AfterRevision check as well.
      AfterRevision := 'revision '+FGitClient.LocalRevision;
      if (FGitClient.LocalRevision<>FRET_UNKNOWN_REVISION) and (BeforeRevisionShort <> FGitClient.LocalRevision) then
        FRepositoryUpdated := true
      else
        FRepositoryUpdated := false;
      Result := true;
    end;
  end;
end;

function TInstaller.DownloadFromSVN(ModuleName: string; var BeforeRevision, AfterRevision: string; UpdateWarnings: TStringList): boolean;
var
  BeforeRevisionShort: string; //Basically the branch revision number
  CheckoutOrUpdateReturnCode: integer;
  DiffFile: String;
  RepoExists: boolean;
begin
  BeforeRevision := 'failure';
  BeforeRevisionShort:='unknown';
  AfterRevision := 'failure';
  FSVNClient.LocalRepository := FBaseDirectory;
  FSVNClient.Repository := FURL;

  RepoExists:=FSVNClient.LocalRepositoryExists;
  if RepoExists then
  begin
    if FSVNClient.LocalRevision=FSVNClient.LocalRevisionWholeRepo then
      BeforeRevision := 'revision '+FSVNClient.LocalRevisionWholeRepo
    else
      BeforeRevision := 'branch revision '+FSVNClient.LocalRevision+' (repository revision '+FSVNClient.LocalRevisionWholeRepo+')';
    BeforeRevisionShort:=FSVNClient.LocalRevision;
  end
  else
  begin
    // We could insist on the repo existing, but then we wouldn't be able to checkout!!
    writelnlog('INFO: directory ' + FBaseDirectory + ' is not an SVN repository (or a repository with the wrong remote URL).');
    if not(DirectoryExistsUTF8(FSVNClient.LocalRepository)) then
    begin
      writelnlog('INFO: creating directory '+FBaseDirectory+' for SVN checkout.');
      ForceDirectoriesUTF8(FBaseDirectory);
    end;
  end;

  if (FSVNClient.LocalRevisionWholeRepo = FRET_UNKNOWN_REVISION) and (FSVNClient.Returncode=FRET_WORKING_COPY_TOO_OLD) then
  begin
    writelnlog('ERROR: The working copy in ' + FBaseDirectory + ' was created with an older, incompatible version of svn.', true);
    writelnlog('  Run svn upgrade in the directory or make sure the original svn executable is the first in the search path.', true);
    result := false;  //fail
    exit;
  end;

  if RepoExists then
  begin
    FSVNClient.LocalModifications(UpdateWarnings); //Get list of modified files
    DiffFile:='';
    if UpdateWarnings.Count > 0 then
    begin
      UpdateWarnings.Insert(0, ModuleName + ': WARNING: found modified files.');
      if FKeepLocalChanges=false then
      begin
        DiffFile:=IncludeTrailingPathDelimiter(FBaseDirectory) + 'REV' + BeforeRevisionShort + '.diff';
        CreateStoreRepositoryDiff(DiffFile, UpdateWarnings,FSVNClient);
        UpdateWarnings.Add(ModuleName + ': reverting before updating.');
        FSVNClient.Revert; //Remove local changes
      end
      else
      begin
        UpdateWarnings.Add(ModuleName + ': leaving modified files as is before updating.');
      end;
    end;
  end;

  FSVNClient.DesiredRevision := FDesiredRevision; //We want to update to this specific revision
  // CheckoutOrUpdate sets result code. We'd like to detect e.g. mixed repositories.
  FSVNClient.CheckOutOrUpdate;
  CheckoutOrUpdateReturnCode := FSVNClient.ReturnCode;
  case CheckoutOrUpdateReturnCode of
    FRET_LOCAL_REMOTE_URL_NOMATCH:
    begin
      FRepositoryUpdated := false;
      Result := false;
      writelnlog('ERROR: repository URL in local directory and remote repository don''t match.', true);
      writelnlog('Local directory: ' + FSVNClient.LocalRepository, true);
      infoln('Have you specified the wrong directory or a directory with an old repository checkout?',etinfo);
    end;
    else
    begin
      // If there are svn errors, return a false result.
      // We used to do a check for the revision, but that does not check the integrity
      // or existence of all files in the svn repo.
      if FSVNClient.LocalRevision=FSVNClient.LocalRevisionWholeRepo then
        AfterRevision := 'revision '+FSVNClient.LocalRevisionWholeRepo
      else
        AfterRevision := 'branch revision '+FSVNClient.LocalRevision+' (repository revision '+FSVNClient.LocalRevisionWholeRepo+')';
      if (FSVNClient.LocalRevision<>FRET_UNKNOWN_REVISION) and (BeforeRevisionShort <> FSVNClient.LocalRevision) then
        FRepositoryUpdated := true
      else
        FRepositoryUpdated := false;

      // Only return success if svn returned return code 0
      Result := (CheckoutOrUpdateReturnCode=0);
      if not Result then
        writelnlog('DownloadFromSVN: SVN gave error code '+inttostr(CheckoutOrUpdateReturnCode));

      if Result and FReApplyLocalChanges and (DiffFile<>'') then
        begin
          UpdateWarnings.Add(ModuleName + ': reapplying local changes.');
          if FPatchCmd='' then
            FPatchCmd:='patch -p0 -i ';
          if ExecuteCommandInDir(FPatchCmd+' '+DiffFile, FBaseDirectory, FVerbose)<>0 then
            begin
              writelnlog('ERROR: Patching with ' + DiffFile + ' failed.', true);
              writelnlog('  Verify the state of the source, correct and rebuild with make.', true);
              Result := false;  //fail
              exit;
            end;
        end;
    end;
  end;
end;

function TInstaller.DownloadSVN: boolean;
const
  SourceURL = 'http://heanet.dl.sourceforge.net/project/win32svn/1.7.2/svn-win32-1.7.2.zip';
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
  OperationSucceeded := true;
  ForceDirectoriesUTF8(FSVNDirectory);
  SVNZip := SysUtils.GetTempFileName + '.zip';
  try
    OperationSucceeded := Download(SourceURL, SVNZip);
  except
    // Deal with timeouts, wrong URLs etc
    on E: Exception do
    begin
      OperationSucceeded := false;
      writelnlog('ERROR: exception ' + E.ClassName + '/' + E.Message + ' downloading SVN client from ' + SourceURL, true);
    end;
  end;

  if OperationSucceeded then
  begin
    // Extract, overwrite
    if ExecuteCommand(FUnzip + ' -o -d ' + FSVNDirectory + ' ' + SVNZip, FVerbose) <> 0 then
    begin
      OperationSucceeded := false;
      writelnlog('DownloadSVN: ERROR: unzip returned result code: ' + IntToStr(ResultCode));
    end;
  end
  else
  begin
    writelnlog('ERROR downloading SVN client from ' + SourceURL, true);
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
begin
  if FVerbose then
  begin
    // Set up initial output
    if Assigned(FLogVerbose)=false then
    begin
      FLogVerbose:=TLogger.Create;
      FLogVerbose.LogFile:=SysUtils.GetTempFileName;
      WritelnLog('Verbose output saved to ' + FLogVerbose.LogFile, false);
    end;
    FLogVerbose.WriteLog(Output,false);
  end;
  DumpConsole(Sender, output);
end;

function TInstaller.FindSVNSubDirs: boolean;
var
  SVNFiles: TStringList;
  OperationSucceeded: boolean;
begin
  //SVNFiles:=TStringList.Create; //No, Findallfiles does that for you!?!?
  SVNFiles := FindAllFiles(FSVNDirectory, 'svn' + GetExeExt, true);
  try
    if SVNFiles.Count > 0 then
    begin
      // Just get first result.
      FSVNClient.RepoExecutable := SVNFiles.Strings[0];
      OperationSucceeded := true;
    end
    else
    begin
      infoln('Could not find svn executable in or under ' + FSVNDirectory,etwarning);
      OperationSucceeded := false;
    end;
  finally
    SVNFiles.Free;
  end;
  Result := OperationSucceeded;
end;


function TInstaller.GetFPCTarget(Native: boolean): string;
var
  processorname, os: string;
begin
  processorname := 'notfound';
  os := processorname;
  os := {$I %FPCTARGETOS%};
  processorname := {$I %FPCTARGETCPU%};
  os := LowerCase(os); //match usage in makefiles
  processorname := LowerCase(processorname); //match usage in makefiles

  if not Native then
  begin
    if FCrossCPU_Target <> '' then
      processorname := FCrossCPU_Target;
    if FCrossOS_Target <> '' then
      os := FCrossOS_Target;
  end;
  Result := processorname + '-' + os;
end;

procedure TInstaller.LogError(Sender: TProcessEx; IsException: boolean);
var
  TempFileName: string;
begin
  TempFileName := SysUtils.GetTempFileName;
  if IsException then
  begin
    WritelnLog('Exception raised running ' + Sender.ResultingCommand, true);
    WritelnLog(Sender.ExceptionInfo, true);
  end
  else
  begin
    writelnlog('ERROR running ' + Sender.ResultingCommand, true);
    writelnlog('Command returned non-zero ExitStatus: ' + IntToStr(Sender.ExitStatus), true);
    writelnlog('Command path set to: ' + Sender.Environment.GetVar(PATHVARNAME), true);
    writelnlog('Command current directory: ' + Sender.CurrentDirectory, true);
    writelnlog('Command output:', true);
    // Dump command output to screen and detailed log
    infoln(Sender.OutputString,etinfo);
    Sender.OutputStrings.SaveToFile(TempFileName);
    WritelnLog('  output logged in ' + TempFileName, false);
  end;
end;

procedure TInstaller.SetPath(NewPath: string; Prepend: boolean);
begin
  if Prepend then
    NewPath := NewPath + PathSeparator + ProcessEx.Environment.GetVar(PATHVARNAME);
  ProcessEx.Environment.SetVar(PATHVARNAME, NewPath);
  if NewPath <> EmptyStr then
  begin
    WritelnLog('External program path:  ' + NewPath, false);
  end;
  if FVerbose then
    infoln('Set path to: ' + NewPath,etdebug);
end;

procedure TInstaller.ProcessError(Sender: TProcessEx; IsException: boolean);
begin
  // Add exception info generated from processex
  FErrorLog.AddStrings(Sender.ExceptionInfoStrings);
end;

procedure TInstaller.WritelnLog(msg: string; ToConsole: boolean);
begin
  if Assigned(FLog) then
  begin
    FLog.WriteLog(msg,ToConsole);
  end;
end;


function TInstaller.GetCompilerInDir(Dir: string): string;
var
  ExeName: string;
begin
  ExeName := 'fpc' + GetExeExt;
  Result := IncludeTrailingBackslash(Dir) + 'bin' + DirectorySeparator + GetFPCTarget(true) + DirectorySeparator + 'fpc';
  {$IFDEF UNIX}
  if FileExistsUTF8(Result + '.sh') then
  begin
    //Use our proxy if it is installed
    Result := Result + '.sh';
  end;
  {$ENDIF UNIX}
end;

constructor TInstaller.Create;
begin
  inherited Create;
  ProcessEx := TProcessEx.Create(nil);
  ProcessEx.OnErrorM := @LogError;
  FGitClient := TGitClient.Create;
  FHGClient := THGClient.Create;
  FSVNClient := TSVNClient.Create;
  // List of binutils that can be downloaded:
  CreateBinutilsList;
  FNeededExecutablesChecked:=false;
  // Set up verbose log: will be done in dumpoutput
  // as it depends on verbosity etc
  //FLogVerbose: TLogger.Create;
  FErrorLog := TStringList.Create;
  ProcessEx.OnErrorM:=@(ProcessError);
end;

destructor TInstaller.Destroy;
begin
  if Assigned(FBinUtils) then
    FBinUtils.Free;
  if Assigned(FLogVerbose) then
    FLogVerbose.Free;
  if Assigned(FErrorLog) then
    FErrorLog.Free;
  if Assigned(ProcessEx.OnErrorM) then
    ProcessEx.OnErrorM:=nil;
  if Assigned(ProcessEx.OnError) then
    ProcessEx.OnError:=nil;
  ProcessEx.Free;
  FGitClient.Free;
  FHGClient.Free;
  FSVNClient.Free;
  inherited Destroy;
end;


end.

